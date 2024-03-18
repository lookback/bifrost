use chrono::{DateTime, Utc};
use serde::de::Error;
use serde::{self, Deserialize, Deserializer, Serializer};
const US_TO_S: f64 = 1_000_000.0;

/// Serialize to unix timestamp
pub fn serialize<S>(date: &DateTime<Utc>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    // NB: This only works with years +/- 292k years from epoch
    let ns: f64 = date.timestamp_micros() as f64;
    // Micros to seconds
    let secs = ns / US_TO_S;

    serializer.serialize_f64(secs)
}

// Deserialize from unix timestamp
pub fn deserialize<'de, D>(deserializer: D) -> Result<DateTime<Utc>, D::Error>
where
    D: Deserializer<'de>,
{
    let secs = f64::deserialize(deserializer)?;
    // NB: This only works with years +/- 292k years from epoch
    let micros = (secs * US_TO_S) as i64;

    DateTime::<Utc>::from_timestamp_micros(micros).ok_or_else(|| {
        D::Error::custom(format!(
            "Unix timestamp out of range {secs}, max +/- ~292k years from UNIX epoch is supported."
        ))
    })
}
