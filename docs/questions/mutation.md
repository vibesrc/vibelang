# Mutation Model

## Summary

Vibelang uses **ownership-based mutation** with two borrow types:

| Syntax | Behaves like | Can mutate? |
|--------|--------------|-------------|
| `x: T` | `let` | Yes (owns it) |
| `x: &T` | `const` | No (read-only borrow) |
| `x: ~T` | `let` | Yes (vibing - mutable borrow) |

## Examples

### Owner Mutates Directly

```vibelang
let arr = Array<i32>()
arr.push(1)                 // we own arr, can mutate
arr.push(2)
```

### Read-Only Borrow (`&`)

```vibelang
fn print_len(arr: &Array<i32>) {
    print(arr.len())        // OK: reading
    arr.push(1)             // ERROR: arr is borrowed as read-only
}

let arr = Array<i32>()
arr.push(1)
print_len(&arr)             // lend read-only
arr.push(2)                 // still own it
```

### Vibing (`~`) - Mutable Borrow

```vibelang
fn add_item(arr: ~Array<i32>, item: i32) {
    arr.push(item)          // OK: arr is vibing (mutable borrow)
}

let arr = Array<i32>()
add_item(~arr, 1)           // vibe with arr
add_item(~arr, 2)           // still own it
print(arr.len())            // 2
```

## Borrow Rules

1. Can have **many `&T`** OR **one `~T`**, not both
2. Borrows cannot outlive the owner
3. Cannot move while borrowed

## Why `~` for Vibing?

- `&` = passive, just looking (read-only)
- `~` = wavy, vibing, doing something (mutable)
- Shorter than Rust's `&mut`
- Fresh symbol, no C/Rust baggage
- It's called Vibelang for a reason 🌊
