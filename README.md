# 箋 Jian

A Wenyan Markdown language concept.

## Install

1. Clone: `git clone https://github.com/Meowcolm024/jian.git`
2. Build: `stack build`
3. Run: `stack run [filename]`

## Examples

| 箋 Jian                                 | Markdown                            |
| :-------------------------------------- | :---------------------------------- |
| [Example.jian](examples/example.jian)   | [Example.md](examples/example.md)   |
| [滕王閣序.jian](examples/滕王閣序.jian) | [滕王閣序.md](examples/滕王閣序.md) |

## Syntax

Currently supported syntax:

### Headings

Headings are done through indentations, and there should **NOT** be punctuations in it.
Every **2** indentations adds one title level.

``` markdown
史記
  本紀
    秦始皇本紀第六
<!--Euqals to-->
# 史記
## 本紀
### 秦始皇本紀第六
```

### Body

A paragraph should only take up **ONE** line and each sentence should end with either `。？！：`.

### Blockquote

Starts with `「「` ends with `」」`

Example:

``` markdown
「「
長太息以掩涕兮，哀民生之多艱。
」」
<!--Euqals to-->
<blockquote>
長太息以掩涕兮，哀民生之多艱。
</blockquote>
```

### List

Ordered lists start with a number (in Hanzi), followed by a `、`, like `一、`, and it ends with `【列終】`.

Examples:

``` markdown
一、文言也
二、Haskell也
三、表之實例也
【列終】
<!--Euqals to-->
1. 文言也
2. Haskell也
3. 表之實例也
```

For unordered lists, use `〇`, and it ends with `【列終】`.

``` markdown
〇《滕王閣序》
【列終】
<!--Euqals to-->
- 《滕王閣序》
```

### Comments

Comments start with `批：`:

``` markdown
批：註釋也
<!--Euqals to-->
<!--批：註釋也-->
```

### Images

Format should be strictly followed: `【有圖者「[name]」自「[url]」來】`

**Notice**: An extra empty line is needed in order to generate a newline.

Example:

``` markdown
【有圖者「清明上河圖」自「zhangzeduan」來】
<!--Euqals to-->
![清明上河圖](zhangzeduan)
```

### URL

Format should be strictly followed: `【有扉者「[name]」通「[url]」也】`

**Notice**: An extra empty line is needed in order to generate a newline.

``` markdown
【有扉者「Github」通「https://github.com」也】
<!--Euqals to-->
[Github](https://github.com)
```

### Inline

Format should be strictly followed: `〔[code]〕`

**Notice**: An extra empty line is needed in order to generate a newline.

Example:

``` markdown
〔putStrLn "Hello"〕
<!--Euqals to-->
`putStrLn "Hello"`
```

## Ideas

| Feature        | Status | Note                                        |
| :------------- | :----- | :------------------------------------------ |
| Heading        | ✓      | Done through indentations                   |
| Ordered List   | ✓      | Starts with `[數字]、` ends with `【列終】` |
| Unordered List | ✓      | Starts with `〇`, ends with `【列終】`      |
| Blockquote     | ✓      | Wrapped in `「「` and `」」`                |
| Image          | ✓      | `【有圖者「[name]」自「[url]」來】`         |
| URL            | ✓      | `【有扉者「[name]」通「[url]」也】`         |
| Comment        | ✓      | Starts with `批：`                          |
| Inline         | ✓      | Wrapped in `〔` and `〕`                    |
| Code Block     | ?      | -                                           |
| Table          | ?      | -                                           |
| Bold           | ?      | -                                           |
| Italic         | 𐄂     | There is no *italic* in Chinese             |
