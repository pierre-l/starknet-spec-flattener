use anyhow::Context;
use serde::de::Error;
use serde::{Deserialize, Deserializer, Serialize};
use serde_json::{Map, Value};

/// An "AllOf" object as represented in the spec.
/// "AllOf" represents multiple objects merged into one, cumulating the properties of all its items.
#[derive(Deserialize)]
pub struct AllOfInput {
    #[serde(rename = "allOf")]
    items: Vec<AllOfItem>,
    // TODO Option is probably better?
    #[serde(default)]
    description: String,
    // TODO Option is probably better?
    #[serde(default)]
    title: String,
}

pub enum AllOfItem {
    Reference {
        description: String,
        title: String,
        reference: (String, Value),
    },
    Custom {
        description: String,
        title: String,
        type_: String,
        properties: Map<String, Value>,
    },
}

impl<'a> Deserialize<'a> for AllOfItem {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'a>,
    {
        let value = Value::deserialize(deserializer)?;
        AllOfItem::try_from(&value).map_err(|err| Error::custom(err.to_string()))
    }
}

impl TryFrom<&Value> for AllOfItem {
    type Error = anyhow::Error;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        let mut obj: Map<String, Value> = value.as_object().unwrap().clone();

        if obj.get("type").is_some() {
            let output = AllOfItem::Custom {
                description: obj
                    .get("description")
                    .unwrap_or(&Value::String("".to_string()))
                    .as_str()
                    .context("\"description\" is supposed to be a string")?
                    .to_string(),
                // TODO Apparently this isn't mandatory? Good, we don't need it anyway.
                title: obj
                    .get("title")
                    .unwrap_or(&Value::String("".to_string()))
                    .as_str()
                    .context("\"title\" is supposed to be a string")?
                    .to_string(),
                type_: obj
                    .get("type")
                    .unwrap()
                    .as_str()
                    .context("\"type\" is supposed to be a string")?
                    .to_string(),
                // TODO Optional?
                properties: obj.get("properties").unwrap().as_object().unwrap().clone(),
            };
            Ok(output)
        } else {
            let title = obj
                .remove("title")
                .unwrap_or(Value::String("".to_string()))
                .as_str()
                .unwrap()
                .to_string();
            let description = obj
                .remove("description")
                .unwrap_or(Value::String("".to_string()))
                .as_str()
                .unwrap()
                .to_string();
            assert_eq!(obj.len(), 1);
            let reference = obj.into_iter().next().unwrap();

            Ok(AllOfItem::Reference {
                description,
                title,
                reference,
            })
        }
    }
}

/// A merged "AllOf", with all of its item properties merged in a single object.
#[derive(Serialize)]
pub struct MergedAllOf {
    properties: Map<String, Value>,
    description: String,
    title: String,
}

impl From<AllOfInput> for MergedAllOf {
    fn from(value: AllOfInput) -> Self {
        let AllOfInput {
            items,
            description,
            title,
        } = value;

        let mut merged_properties = Map::new();

        items.into_iter().for_each(|item| {
            match item {
                AllOfItem::Reference {
                    description: _,
                    title: _,
                    reference: (key, value),
                } => {
                    merged_properties.insert(key, value);
                }
                AllOfItem::Custom {
                    description: _,
                    title: _,
                    type_,
                    mut properties,
                } => {
                    assert_eq!(type_, "object");

                    // TODO Keep?
                    if properties.iter().any(|(key, _)| {
                        ["description", "title", "required"].contains(&key.as_str())
                    }) {
                        panic!("Reserved name already in use")
                    }

                    // TODO Check pre-existence
                    // Now add these to the merged props
                    merged_properties.append(&mut properties);
                }
            }
        });

        Self {
            properties: merged_properties,
            description,
            title,
        }
    }
}
