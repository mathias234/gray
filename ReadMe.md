# Gray
An interpreted scripting language written in Rust. Not yet ready for use

## Examples
### Variables
#### Simple Variable
Variables as in most languages will store a single value
```
let a = 0;
```

#### Object Declaration
Objects are simple structures that can hold values based with keys with the syntax `key: value,`
```
let a = {
    member1: 50,
    member2: "Hello World",
};

print("member1 is {}", a.member1);
```

#### Array Declaration
Arrays are a collection of arbitrary values
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
Ranges are a special syntax that under the hood will construct an RangeIterator which can be iterated over with a `for` loop.
```
for i in 0..5 {
    print("Index is {}", i);
}
```

#### For loop array based
Using the `for` keyword will allow you to iterate any Array object to get it's values
```
let array = [0, 1, 2, 3, 4]

for v in array {
    print("Value is {}", v);
}
```
### Functions
#### Normal function
A normal function as seen in most programming languages.
```
fn test(arg1, arg2, arg3) {
    print("{}, {}, {}", arg1, arg2, arg3);
}

test(0, 1, 2);
```

#### Functions with variable arguments
`params` is a special keyword for doing variable arguments, 
it will assign all the arguments after it into an Array object.
```
fn test(params) {
    for param in params {
        print("{}", param);
    }
}

test(0, 1, 2);
```
