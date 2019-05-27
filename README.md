bifrost
=======

GraphQL -> structs/JSON bindings

## Building

```
$ rustup update
$ cargo build
```


## Rust bindings

```
$ ./target/debug/bifrost rust ./schema.gql -t User
```

## Pass-through (for debugging)

```
$ ./target/debug/bifrost pass ./schema.gql -t User
```
