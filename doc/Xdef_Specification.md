# xdefの言語仕様
これは、xdefの言語仕様を説明するドキュメントです。
XMLとXML Schemaの知識を持っている方を対象読者とします。

## 用語
### ノード
XMLにおける要素と属性の総称を`ノード`と呼びます。
`ノード`は、値・子ノードを持つことができます。

例えば、下記の`elm`要素の値は`elmValue`、`attr`属性の値は`attrValue`です。

```xml
<elm attr="attrValue">elmValue</elm>
```

下記は、`parent`要素が子ノードとして`child1`属性と`child2`要素を持っています。

```xml
<parent child1="hoge">
  <child2>piyo</child2>
</parent>
```

要素は、値と子ノードを同時に持つことができますが、値を持つ場合は、属性のみ子ノードとして持てます。
属性は、子ノードを持つことはできず、値だけを持つことができます。

### 許容される
あるスキーマに対して、XML・ノード・値が妥当であることを`許容される`と呼びます。

### 型
ノードの内容として、許容される値の定義を`型`と呼びます。

### 宣言
許容されるノードの定義を、`宣言`と呼びます。

## Xdefとは
Xdefは、XML Schemaを簡潔に記述するために作られた言語です。
XML Schemaの主要な機能を、Xdefで表現することができます。
Xdefのファイル形式は、`.xdef`です。
エンコーディング形式は、`UTF-8`のみを使用することができます。

## ノードの宣言
ノードの宣言をすることによって、許容されるXMLのノードを定義することができます。

### 単純型要素の宣言
下記のようなxdefがあります。

```xdef
Root : string
```

このxdefは、下記のようなXMLを許容します。

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Root>Hello World!</Root>
```

Xdefファイル中の、`Root`は許容される要素の名前です。
`: string`は、`Root`要素が文字列を定義する`string`型であることを表します。
このように子ノードを持たない要素の宣言を、単純型要素の宣言と呼びます。

### 複雑型要素の宣言
下記のようなxdefがあります。

```xdef
Root
  Child : string
```

このxdefファイルは、下記のようなXMLを許容します。

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Root>
  <Child>
    Hello World!
  </Child>
</Root>
```

`Child`は`Root`の子要素です。
このようにインデントを使用することで、ノードの階層を表現することができます。
半角スペース2つで、一つのインデントを表します。
このように子ノードを持つ要素の宣言を、複雑型要素の宣言と呼びます。

下記のように、複雑型要素の宣言は、要素の宣言以外に、属性の宣言を入れ子にすることもできます。

```xdef
Root
  @Attr : string
  Child : string
```

このxdefファイルは、下記のようなXMLを許容します。

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Root Attr="attrValue">
  <Child>
    Hello World!
  </Child>
</Root>
```

#### パーティクルの指定
複雑型要素の宣言では、`::`を使用することでパーティクルの指定をすることができます。
パーティクルの指定をすることで、子ノードの出現や順番を指定することができます。
パーティクルの指定は省略可能です。その場合は、`sequence`を指定したことになります。
パーティクルの指定には、下記の種類があります。

| パーティクルの指定 |
|--------------------|
| sequence           |
| choice             |
| all                |

例えば、下記はパーティクルの指定として、`all`を使用しています。

```xdef
Root :: choice
  Child1 : string
  Child2 : string
```

このxdefファイルは、下記の2つXMLをどちらも許容します。

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Root>
  <Child1>
    Hello World!
  </Child1>
</Root>
```

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Root>
  <Child2>
    Hello World!
  </Child2>
</Root>
```

パーティクルの指定に関しての詳しい情報は、[ModelGroups](http://www.w3.org/TR/xmlschema-1/#Model_Groups)を参照してください。

### 出現回数の指定
要素の宣言・パーティクルの指定には、出現回数を指定することができます。

| 記号   | 説明                                                                                                           |
|--------|----------------------------------------------------------------------------------------------------------------|
| なし   | 出現回数の指定がない場合は、宣言の対象となるノードは必須になります。                                           |
| ?      | 省略可能を表します。                                                                                           |
| *      | 任意回数を表します。                                                                                           |
| +      | 一つ以上を表します。                                                                                           |
| [n..m] | nとmには自然数を指定します(例: [0..10]) n以上m以下を表します。nかmに * を指定した場合は、無制限になります。    |

例えば、下記は、`Root`要素の出現回数を1個から3個と指定しています。

```xdef
Root[1..3]
  Child : string
```

要素に対してではなく、パーティクルに対しての出現回数の指定は下記のように行います。

```xdef
Root :: sequence[1..3]
  Child : string
```

要素の宣言に対する出現回数の指定は、XML Schemaでは、[minOccurs属性](http://www.w3.org/TR/xmlschema-0/#attribute-minOccurs)や[maxOccurs属性](http://www.w3.org/TR/xmlschema-0/#attribute-maxOccurs)として表現されます。
属性の宣言では、[use属性](http://www.w3.org/TR/xmlschema-0/#attribute-use)として表現されます。

## 型
ノードの宣言において、`:`で型を指定することができます。
XML Schemaで定義済みの型(単純型)が、そのまま使用できることに加え、制限型というrestrictionを使用するような型を、表現することができます。
下記は、それぞれの使用方法を説明します。

### 単純型
単純型は、XML Schemaで定義済みの型を表します。
使用できる型は、[Built-in datatypes](http://www.w3.org/TR/xmlschema-2/#built-in-datatypes)を参照してください。

### 制限型
制限型は、XML Schemaの[restriction](http://www.w3schools.com/schema/schema_facets.asp)によって、制限される型を表します。

下記は、その種類と例です。

| 種類         | 例                                  |
|--------------|-------------------------------------|
| 正規表現     | `/\w+/`                             |
| 文字列列挙   | `("aaa" | "bbb")`                   |
| 範囲指定整数 | `[0, 100)` `[0, 100]`               |
| 可変長文字列 | `char[1..100]` `char[1..*]`         |
| 固定長文字列 | `char` `char[100]`                  |

### 固定型
固定型は、単一の値のみ許容される型を表します。

下記は、その種類と例です。

| 種類         | 例                                  |
|--------------|-------------------------------------|
| 文字列       | `"hoge"`                            |
| 真偽         | `true` `false`                      |
| 整数         | `-100` `0` `100`                    |
| 浮動小数点数 | `-100.001` `100.001`                |
| バイト       | `-100y` `0y` `100y`                 |

XML Schemeの[fixed](http://www.w3.org/TR/xmlschema-0/#attribute-fixed)や、単一の[enumeration](http://www.w3schools.com/schema/schema_facets.asp)によって表現されます。

### 型の定義
`=`を使用することで再利用可能な型の定義をすることができます。
型の定義には、単純型と複雑型の2種類があります。

#### 単純型の定義
単純型の定義は、下記のように行います。

```xdef
MyString = char[100]
```

この例は、`MyString`という型を定義しています。
この型の定義は`char[100]`なので、長さ100の固定長文字列です。

#### 複雑型の定義

複雑型の定義は、下記のように行います。

```xdef
MyType =
  ChildA : string
```

この例は、`MyType`という型を定義しています。
この型は、子要素の宣言として`ChildA`を持ちます。

`MyType`のパーティクルを明示的に指定するには下記のようにします。

```xdef
MyType :: choice =
  ChildA : string
```

省略されている場合は、`sequence`になります。

## ノードジェネレータ
通常のノードの宣言では任意の要素を許容するanyを宣言することができません。
この例のように、通常のノード宣言では表せないノードを宣言をしたい場合は、ノードジェネレータを使用します。
ノードジェネレータは、下記のように`!`を使用して呼び出すことができます。

```xdef
!any
```

xdefでは、anyの他にも数種類のノードジェネレータが用意されています。
例えば`include`ノードジェネレータは、XML Schemaの[incldue](http://www.w3schools.com/schema/el_include.asp)を表現します。

下記のように使用します。

```xdef
!include "http://www.w3.org/2001/xml.xsd"
```

`include`ノードジェネレータは固定文字列型の引数を１つ受け取ります。


下記は、使用できるノードジェネレータの説明です。

| ノードジェネレータ名 | 説明                                                                                           | 引数                 | 出現回数     | 子ノード      |
|----------------------|------------------------------------------------------------------------------------------------|----------------------|--------------|---------------|
| include              | XML Schemaの[incldue](http://www.w3schools.com/schema/el_include.asp)を表現します。            | 固定文字列           | 受け取らない | 受け取らない  |
| targetNamespace      | XML Schemaの[targetNamespace](http://www.w3schools.com/schema/el_include.asp)を表現します。    | 固定文字列           | 受け取らない | 受け取らない  |
| choice               | 入れ子になるchoiceを表現します。                                                               | 受け取らない         | 受け取る     | 受け取る      |
| any                  | XML Schemaの[any](http://www.w3schools.com/schema/schema_complex_any.asp)を表現します。        | 受け取らない         | 受け取る     | 受け取らない  |
| element              | 定義済みである要素の宣言を名前を指定して宣言します。                                           | 固定文字列           | 受け取る     | 受け取る      |

## コメント
ノードの宣言にはコメントをつけることができます。
下記の例のように行末に`--`をつけます。

```xdef
Root : string -- コメントです。
```
