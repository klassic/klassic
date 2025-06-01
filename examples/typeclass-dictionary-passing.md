# Type Class Dictionary Passing Implementation

This document explains how type classes are implemented in Klassic using dictionary passing.

## Type Class Declaration

```klassic
typeclass Show<'a> where {
  show: ('a) => String;
}
```

This declares a type class `Show` with a single method `show`.

## Instance Declaration

```klassic
instance Show<Int> where {
  def show(x: Int): String = "Int(" + x + ")"
}

instance Show<String> where {
  def show(x: String): String = "\"" + x + "\""
}
```

## Dictionary Transformation

The compiler transforms these declarations into:

```klassic
// Generated dictionary for Show<Int>
val Show_Int_dict = {
  show: (x: Int) => "Int(" + x + ")"
}

// Generated dictionary for Show<String>
val Show_String_dict = {
  show: (x: String) => "\"" + x + "\""
}
```

## Function with Type Class Constraint

```klassic
def display<'a>(x: 'a): String where Show<'a> = {
  show(x)
}
```

This is transformed to:

```klassic
def display<'a>(Show_dict: { show: ('a) => String }, x: 'a): String = {
  Show_dict.show(x)
}
```

## Method Call Resolution

When you call:

```klassic
display(42)
display("Hello")
```

The compiler transforms these to:

```klassic
display(Show_Int_dict, 42)
display(Show_String_dict, "Hello")
```

## Multiple Constraints

```klassic
def showAndCompare<'a>(x: 'a, y: 'a): String where (Show<'a>, Eq<'a>) = {
  if (equals(x, y)) {
    show(x) + " equals " + show(y)
  } else {
    show(x) + " not equals " + show(y)
  }
}
```

Transforms to:

```klassic
def showAndCompare<'a>(
  Show_dict: { show: ('a) => String },
  Eq_dict: { equals: ('a, 'a) => Boolean },
  x: 'a, 
  y: 'a
): String = {
  if (Eq_dict.equals(x, y)) {
    Show_dict.show(x) + " equals " + Show_dict.show(y)
  } else {
    Show_dict.show(x) + " not equals " + Show_dict.show(y)
  }
}
```

## Implementation Details

1. **Type Class Methods**: When a type class method like `show` is called, the compiler looks up which type class it belongs to.

2. **Instance Resolution**: Based on the type of the first argument, the compiler finds the matching instance.

3. **Dictionary Access**: The method call is transformed to access the method from the appropriate dictionary.

4. **Compile-Time Resolution**: All instance resolution happens at compile time, so there's no runtime overhead for looking up instances.

## Benefits

- **Type Safety**: The compiler ensures that instances exist for all used type classes
- **Modularity**: New instances can be added without modifying existing code
- **Performance**: Dictionary passing has minimal runtime overhead
- **Clarity**: The transformation is straightforward and predictable