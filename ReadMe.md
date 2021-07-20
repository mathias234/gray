# Gray
An interpreted scripting language written in Rust. Not yet ready for use

## Examples
### Variables
#### Simple Variable
```
let a = 0;
```

#### Object Declaration
```
let a = {
    member1: 50,
    member2: "Hello World",
};
```

#### Array Declaration
```
let a = [0, "Hello World", 500];
```


### Comparisons
```
let a = 5;
if a > 4 && a < 6 {
    print("{}", a);
}
```

### Loops
#### For loop range based
```
for i in 0..5 {
    print("Index is {}", i);
}

```

#### For loop array based
```
let array = [0, 1, 2, 3, 4]

for v in array {
    print("Value is {}", v);
}
```
