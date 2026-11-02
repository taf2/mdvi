# Rendering showcase

Open this file with `make run ARGS='examples/rendering.md'`. Resize the terminal
while reading: paragraphs should wrap without hiding later content or shifting
the cursor away from the text you were reading.

## Typography and links

Regular text, **bold**, *italic*, ~~strikethrough~~, and `inline code` can share a
line. A [named link](https://example.com) shows its destination; an autolink such
as <https://example.com> appears only once.

Unicode: café, 界面, é, and 👩‍💻. Move across these characters with `h` and `l`,
or select them with `Ctrl-v` and copy with `y`.

## Tables

| Feature | Status | Count |
| :--- | :---: | ---: |
| **Tables** | ready | 12 |
| 界面 | ready | 3 |
| Empty cell | | 0 |

## Lists and quotations

- A parent item
  - A nested child
  - Another child
- A sibling without an extra blank row

1. First step
2. Second step

- [x] Completed task
- [ ] Remaining task

> `Quoted code` and **bold text**.
>
> > A nested quotation.
>
> - A list inside a quote

## Code

```rust
// This comment must not change the highlighting of the next line.
fn main() {
    let greeting = "Hello, 界面";

    println!("{greeting}");
}
```

## Long paragraphs

This paragraph deliberately spans several terminal rows. Search for needle near the end of this sentence and the viewer should reveal the wrapped row containing needle. Page down, jump to the end with G, then resize the terminal and confirm that the document remains readable. Long words also remain accessible: abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789.

The final paragraph should remain reachable at every terminal width.
