# 箋 Jian

A prototype of a Wen-yan Markdown language concept.

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

### Titles

3 levels are supported:

- h1: `《[title]書》`
- h2: `《[title]卷》`
- h3: `《[title]篇》`

Examples:

``` markdown
《範例之書》
<!--Euqals to-->
# 範例之書

《介紹卷》
<!--Euqals to-->
## 介紹卷

《離騷篇》
<!--Euqals to-->
### 離騷篇
```

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

Lists start with a number (in Hanzi), followed by a `、`, like `一、`

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

### Comments

``` markdown
批：註釋也
<!--Euqals to-->
<!--批：註釋也-->
```

### Images

Format should be strictly followed: `【有圖者「[name]」自「[url]」來】`

**Notice**: You need to use spaces to separate image block and other text or simply put it in a new line.
(check out the second image in example)

Example:

``` markdown
【有圖者「清明上河圖」自「zhangzeduan」來】
<!--Euqals to-->
![清明上河圖](zhangzeduan)
```

### URL

Format should be strictly followed: `【有扉者「[name]」往「[url]」也】`

**Notice**: You need to use spaces to separate URL block and other text or simply put it in a new line.

``` markdown
【有扉者「Github」往「https://github.com」也】
<!--Euqals to-->
[Github](https://github.com)
```

## Ideas

### Code Block

No idea :(
