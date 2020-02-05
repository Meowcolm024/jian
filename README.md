# 箋 Jian

A prototype of a Wenyan Markdown language concept.

And the code is poorly written :)

## Install

1. Clone: `git clone https://github.com/Meowcolm024/jian.git`
2. Build: `stack build`
3. Run: `stack run [filename]`

## Examples

| 箋 Jian                      | Markdown                 |
| :--------------------------- | :----------------------- |
| [Example.jian](example.jian) | [Example.md](example.md) |

## Syntax

Currently supported syntax:

### Headings

Headings are done through indentations, and there should **NOT** be punctuations in it.

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

A paragraph should only take up **ONE** line and ends with a `。`.

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

Ordered lists start with a number (in Hanzi), followed by a `、`, like `一、`

Examples:

``` markdown
一、文言也
二、Haskell也
三、表之實例也
<!--Euqals to-->
1. 文言也
2. Haskell也
3. 表之實例也
```

For unordered lists, use `〇` (text should follow directly without leaving a space)

``` markdown
〇《滕王閣序》
<!--Euqals to-->
- 《滕王閣序》
```

### Comments

**Notice**: There should not be spaces in the comment line (otherwise it may not work...)

``` markdown
批：註釋也
<!--Euqals to-->
<!--批：註釋也-->
```

### Images

Format should be strictly followed: `【有圖者「[name]」自「[url]」來】`

**Notice**: Should not be put inline!

Example:

``` markdown
【有圖者「清明上河圖」自「zhangzeduan」來】
<!--Euqals to-->
![清明上河圖](zhangzeduan)
```

### URL

Format should be strictly followed: `【有扉者「[name]」通「[url]」也】`

**Notice**: Should not be put inline!

``` markdown
【有扉者「Github」通「https://github.com」也】
<!--Euqals to-->
[Github](https://github.com)
```

## Ideas

| Feature    | Status                           | Reason                          |
| :--------- | :------------------------------- | :------------------------------ |
| Inline     | Pending                          | -                               |
| Code Block | Pending                          | -                               |
| Table      | Pending                          | -                               |
| Bold       | Pending                          | -                               |
| Italic     | <font color=red> Ignored </font> | There is no *italic* in Chinese |
