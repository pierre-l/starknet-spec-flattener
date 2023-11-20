mod all_of;

use crate::all_of::{AllOfInput, MergedAllOf};
use anyhow::{Context, bail};
use std::{fmt::Display, collections::HashSet};
use serde_json::{Map, Value};
use std::collections::HashMap;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let mut registry = SchemaRegistry::new();

    process(&mut registry, Domain::Api).await?;
    process(&mut registry, Domain::WriteApi).await?;
    process(&mut registry, Domain::TraceApi).await?;

    // TODO Hunt down panics? unwrap, expect, panic

    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Domain {
    Api,
    WriteApi,
    TraceApi,
}

impl Domain {
    fn name(&self) -> &'static str {
        match self {
            Self::Api => "api_openrpc",
            Self::WriteApi => "write_api",
            Self::TraceApi => "trace_api_openrpc",
        }
    }

    fn file_name(&self) -> String {
        format!("starknet_{}.json", self.name())
    }
}

impl Display for Domain {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

struct SchemaRegistry {
    schemas: HashMap<Pointer, Value>,
}

impl SchemaRegistry {
    fn new() -> Self {
        Self {
            schemas: HashMap::new(),
        }
    }

    fn insert(&mut self, pointer: Pointer, schema: Value) -> anyhow::Result<()> {
        if self.schemas.insert(pointer.clone(), schema).is_some() {
            bail!("A schema was replaced, indicating a duplicated definition. Or a bug :) Pointer: {:?}", pointer)
        }

        Ok(())
    }

    fn get(&self, pointer: &Pointer) -> Option<&Value> {
        self.schemas.get(pointer)
    }
}

/// The parsed value of a "$ref" field.
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
struct Pointer {
    domain: Domain,
    path: String,
}

impl Pointer {
    fn try_new(current_domain: Domain, raw_pointer: &str) -> anyhow::Result<Self> {
        // TODO Unit tests
        if raw_pointer.starts_with('#') {
            // The pointer is referencing another schema from the same spec file.
            Ok(Pointer {
                domain: current_domain,
                path: raw_pointer.split_at(1).1.to_string(),
            })
        } else if raw_pointer.starts_with("./") {
            // The pointer is referencing a schema from a different spec file.
            let chunks = raw_pointer.split('#').collect::<Vec<&str>>();
            assert_eq!(2, chunks.len());

            let file_path = chunks[0];
            let schema_path = chunks[1];

            let domain = {              
                if file_path.contains(&Domain::Api.file_name()) {
                    Domain::Api
                } else if file_path.contains(&Domain::TraceApi.file_name()) {
                    Domain::TraceApi
                } else if file_path.contains(&Domain::WriteApi.file_name()) {
                    Domain::WriteApi
                } else {
                    bail!("Unknown domain")
                }
            };

            Ok(Pointer {
                domain,
                path: schema_path.to_string(),
            })
        } else {
            bail!("Unsupported pointer type: {}", raw_pointer)
        }
    }

    fn schema_name(&self) -> &str {
        self.path.split('/').last().unwrap()
    }
}

// TODO Doc
async fn process(registry: &mut SchemaRegistry, domain: Domain) -> anyhow::Result<()> {
    let version = "v0.5.0";
    let domain_name = domain.name();
    let url = format!(
        "https://raw.githubusercontent.com/starkware-libs/starknet-specs/{}/api/{}",
        version, domain.file_name()
    );

    let mut root = fetch_spec_file(&url).await?;
    write_to_file(format!("{domain_name}/0-original/{}", domain.file_name()), &root)?;

    // Flatten the $ref pointers into objects, retrieve a pointer->definition map.
    flatten_openrpc_spec(registry, domain, &mut root)?;

    let flattened_schemas = {
        // TODO Collect?
        let mut result = Map::new();
        registry.schemas.iter()
        .filter(|(pointer, _schema)| pointer.domain == domain)
        .for_each(|(pointer, schema)| {
            result.insert(pointer.path.clone(), schema.clone());
        });
        result
    };

    // Sort the top-level fields so they're ordered by pointer (section, then name)
    let sorted = sort_map_fields(flattened_schemas);
    write_output(domain, "/1-flatten", sorted.clone())?;

    // Trim the "$ref" layer, getting rid of the original "$ref" field, leaving just the pointer object
    let trimmed = {
        let mut object_as_value = Value::Object(sorted);
        for_each_object(&mut object_as_value, &|object| {
            trim_ref_layer(object).unwrap();
        });
        object_as_value.as_object().unwrap().clone()
    };
    write_output(domain, "2-trimmed", trimmed.clone())?;

    // For every object that has a "required" array field, embed that as a boolean in the property objects.
    let mut embedded_required = {
        let mut object_as_value = Value::Object(trimmed);
        for_each_object(&mut object_as_value, &embed_required);
        object_as_value.as_object().unwrap().clone()
    };
    write_output(domain, "3-embedded-required", embedded_required.clone())?;

    // Merge the "allOf" arrays, regroup their item properties into a single object (name -> property)
    let merged_allof = {
        embedded_required.iter_mut().for_each(|(_key, value)| {
            for_each_object(value, &|object| {
                merge_allofs(object).unwrap();
            })
        });
        embedded_required
    };
    write_output(domain, "4-merged-allOf", merged_allof)
}

fn write_output(domain: Domain, step_name: &str, result: Map<String, Value>) -> anyhow::Result<()> {
    let directory = format!("{}/{}", domain.name(), step_name);
    // Write the method files
    result
        .iter()
        .filter_map(|(pointer, schema)| {
            if pointer.starts_with("/methods") {
                let name = pointer.split('/').last().unwrap_or(pointer);
                Some((name, schema))
            } else {
                None
            }
        })
        .for_each(|(name, schema)| {
            write_to_file(format!("{}/methods/{}.json", directory, name), schema).unwrap()
        });

    // Write the whole file
    write_to_file(format!("{}/{}", directory, domain.file_name()), &Value::Object(result))
}

/// Reconstructs the `Map`, sorting the fields by their key.
fn sort_map_fields(flattened_schemas: Map<String, Value>) -> Map<String, Value> {
    let mut raw: Vec<(String, Value)> = flattened_schemas.into_iter().collect();
    raw.sort_by(|(name_a, _), (name_b, _)| name_a.cmp(name_b));
    serde_json::Map::from_iter(raw)
}

fn flatten_openrpc_spec(registry: &mut SchemaRegistry, domain: Domain, root: &mut Value) -> anyhow::Result<()> {
    flatten_refs(registry, domain, root, "/components/errors")?;
    flatten_refs(registry, domain, root, "/components/schemas")?;

    {
        // The methods section is an array. Make it an object. Use the method name as the key.
        let mut object = serde_json::Map::new();
        root.pointer("/methods")
            .context("The spec file must have a methods section")?
            .as_array()
            .context("The methods section must be an array")?
            .iter()
            .for_each(|value| {
                object.insert(
                    value
                        .get("name")
                        .expect("Methods must have a name")
                        .as_str()
                        .expect("Method names must be strings")
                        .to_string(),
                    value.clone(),
                );
            });
        *root
            .pointer_mut("/methods")
            .context("The spec file must have a methods section")? = Value::Object(object);
    }
    flatten_refs(registry, domain, root, "/methods")?;
    Ok(())
}

async fn fetch_spec_file(url: &str) -> anyhow::Result<Value> {
    println!("Fetching {}", url);
    let spec = reqwest::get(url)
        .await
        .context("Failed to retrieve the spec file")?
        .text()
        .await
        .context("Failed to parse the spec file to a string")?;

    serde_json::from_str::<Value>(&spec).context("The spec file isn't valid JSON")
}

/// Trim: remove the intermediate layers of flattened references: the "$ref" field, the pointer object.
///
/// Example input:
/// ```json
/// {
///     "$ref": {
///         "POINTER_OBJECT_NAME": {
///             "pointer_object_field": "value"
///         }
///     },
///     "other_field": "value"
/// }
/// ```
///
/// Example output:
/// ```json
/// {
///     "POINTER_OBJECT_NAME": {
///         "pointer_object_field": "value"
///     },
///     "other_field": "value"
/// }
/// ```
///
/// TODO Unit test
fn trim_ref_layer(obj: &mut Map<String, Value>) -> anyhow::Result<()> {
    for (key, value) in obj.clone() {
        if key == "$ref" {
            // References should now be an object with a single field whose key is the pointer.
            let pointer_object = value
                .as_object()
                .context("\"$ref\" fields are expected to be objects at this point")?;
            // TODO Untrue now, cycle objects
            // assert_eq!(pointer_object.keys().len(), 1);
            let (pointer, flat) = pointer_object
                .iter()
                .next()
                .context("Shouldn't happen as the vec len is supposedly checked above")?;
            // Remove the original reference, the "$ref" field.
            obj.remove(&key);
            // Replace it with the pointer object, effectively removing the "$ref" layer
            let pointer_end = pointer.split('/').last().unwrap_or(pointer).to_string();
            obj.insert(pointer_end, flat.clone());
        }
    }

    Ok(())
}

/// When an object has a "properties" field and a "required" array field, embed the "required" prop
/// as a boolean inside the corresponding property object.
fn embed_required(obj: &mut Map<String, Value>) {
    let Some(required) = obj.get("required").and_then(|v| v.as_array().cloned()) else {
        return;
    };

    let Some(properties) = obj
        .get_mut("properties")
        .and_then(|value| value.as_object_mut())
    else {
        return;
    };

    required.into_iter().for_each(|property_name| {
        let child_prop = properties
            .get_mut(
                property_name
                    .as_str()
                    .context("Property names in the required array are supposed to be strings")
                    .unwrap(),
            )
            .expect("A property marked as \"required\" should already exist in the property map")
            .as_object_mut()
            .expect("Properties are expected to be objects");
        child_prop.insert("required".to_string(), Value::Bool(true));
    });

    obj.remove("required");
}

fn merge_allofs(obj: &mut Map<String, Value>) -> anyhow::Result<()> {
    for (key, value) in obj.clone() {
        if value.get("allOf").is_some() {
            let allof = serde_json::from_value::<AllOfInput>(value.clone())
                .context("allOf deserialization failed")?;
            let merged = MergedAllOf::from(allof);
            obj.insert(
                key,
                serde_json::to_value(&merged).context("Merged allOf serialization failed")?,
            );
        }
    }

    Ok(())
}

fn write_to_file(path: impl AsRef<str>, value: &Value) -> anyhow::Result<()> {
    let path = path.as_ref();
    std::fs::write(
        &path,
        serde_json::to_string_pretty(&value).context("Serialization failed")?,
    )
    .context(format!("Failed to write to file: {}", path))
}

/// Flattens the "$ref" fields.
/// "$ref" fields originally are strings formatted like this: `"$ref": "#/components/schemas/EVENT"`
/// This function lists all schemas, and for every "$ref", replaces the pointer string with the schema object.
/// By doing this iteratively, eventually all the schemas gets fully flat and all "$ref" fields contain objects.
///
/// # Panics
/// This function will panic in two main cases:
/// * the values don't fit the expected schema (eg: $refs are anything else than strings and objects,
///     or the pointer value is formatted in an unexpected way)
/// * a flattening pass doesn't result in fewer schemas to flatten (eg: there's a $ref cycle).
fn flatten_refs(registry: &mut SchemaRegistry, domain: Domain, root: &Value, pointer: &str) -> anyhow::Result<()> {
    let reference_prefix = "#".to_string() + pointer + "/";
    let mut schemas = root.pointer(pointer).context("Pointer not found")?.as_object().context("Pointer does not point at an object")?.clone();

    let mut schemas_left = usize::MAX;
    while schemas_left > 0 {
        // Collect the names of the flat schemas
        let flat = schemas
            .iter_mut()
            .filter_map(|(name, definition)| {
                if !leaf_fields(definition)
                    .into_iter()
                    .any(|(key, value)| key == "$ref" && value.as_str().is_some())
                {
                    Some(name.clone())
                } else if name == "FUNCTION_INVOCATION"{
                    // Recursive type, skip.
                    *definition = Value::String("_RECURSIVE_TYPE_SKIPPED_".to_string());
                    Some(name.clone())
                } else {
                    None
                }
            })
            .collect::<Vec<String>>();

        for flat in flat {
            let definition = schemas.remove(flat.as_str()).context("No way we can't find the name of a schema that we just collected. That's a bug.")?;
            let raw_pointer = reference_prefix.clone()+&flat;
            registry.insert(Pointer::try_new(domain, &raw_pointer)?, definition)?;
        }

        let schema_names = schemas.keys().cloned().collect::<HashSet<String>>();
        // Perform a flattening pass: if a "$ref" is mentioned, make it a pointer object.
        for schema in schemas.values_mut() {
            leaf_fields(schema)
                .into_iter()
                .for_each(|(key, value)| {
                    if key == "$ref" {
                        let raw_pointer = value.as_str().unwrap();
                        let pointer = Pointer::try_new(domain, raw_pointer).unwrap();

                        if let Some(definition) = registry.get(&pointer) {
                            let mut object = serde_json::Map::new();
                            object
                                .insert(raw_pointer.to_string(), definition.clone());
                            *value = Value::Object(object);
                        } else if pointer.domain == domain && !schema_names.contains(pointer.schema_name()) {
                            // Local schema not found.
                            let mut object = Map::new();
                            println!("Schema not found: {:?}", pointer);
                            object.insert("_SCHEMA_NOT_FOUND_".to_string(), Value::String(raw_pointer.to_string()));
                            *value = Value::Object(object);
                        }
                    }
                })
        }

        if schemas_left == schemas.len() {
            let schema_names = schemas.keys().map(|name| "\n".to_string() + name).collect::<String>();
            bail!("No replacement during the last pass. Cycle? Schemas left: {}", schema_names);
        }
        schemas_left = schemas.len();
    }

    Ok(())
}

/// Returns mutable references to the leaf fields of all `Value::Object`s in the tree.
/// Leaf values are `Value::Null`, `Value::Bool`, `Value::Number` and `Value::String`.
///
/// ⚠ returns an empty array if the input is already a leaf field.
fn leaf_fields(value: &mut Value) -> Vec<(&str, &mut Value)> {
    match value {
        Value::Null | Value::Bool(_) | Value::Number(_) | Value::String(_) => {
            vec![]
        }
        Value::Array(array) => array.iter_mut().flat_map(leaf_fields).collect(),
        Value::Object(obj) => obj
            .iter_mut()
            .flat_map(|(key, value)| match value {
                Value::Object(_) => leaf_fields(value),
                Value::Array(array) => array.iter_mut().flat_map(leaf_fields).collect(),
                Value::Null | Value::Bool(_) | Value::Number(_) | Value::String(_) => {
                    vec![(key.as_str(), value)]
                }
            })
            .collect(),
    }
}

/// Apply the closure to every `Value::Object` in the tree.
fn for_each_object<F>(value: &mut Value, f: &F)
where
    F: Fn(&mut Map<String, Value>),
{
    match value {
        Value::Null | Value::Bool(_) | Value::Number(_) | Value::String(_) => {}
        Value::Array(array) => array.iter_mut().for_each(|v| for_each_object(v, f)),
        Value::Object(obj) => {
            obj.iter_mut()
                .for_each(|(_key, value)| for_each_object(value, f));

            f(obj)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{embed_required, leaf_fields};
    use serde_json::{json, Value};

    #[test]
    fn test_leaf_fields() {
        let mut value = json!({
            "field1": {
                "subfield1": {
                    "subsubfield1": "subsubvalue1"
                }
            },
            "field2": "value2"
        });

        assert_eq!(
            leaf_fields(&mut value),
            vec![
                ("subsubfield1", &mut json!("subsubvalue1")),
                ("field2", &mut json!("value2"))
            ]
        )
    }

    #[test]
    fn test_embed_required() {
        let mut original = json!({
            "properties": {
                "block_hash": {
                    "BLOCK_HASH": "omitted",
                    "title": "Block hash"
                }
            },
            "required": [
                "block_hash"
            ],
            "title": "Block hash",
            "type": "object"
        });
        let expected = json!({
            "properties": {
                "block_hash": {
                    "BLOCK_HASH": "omitted",
                    "title": "Block hash",
                    "required": true
                }
            },
            "title": "Block hash",
            "type": "object"
        });

        let mut result = original.as_object().unwrap().clone();
        embed_required(&mut result);
        assert_eq!(Value::Object(result), expected)
    }
}
