## Primitives to quickly make structured formats

import std/[
  strutils,
]


const
  TermChars* = {'(', ')', ';'} # delimiters in the format (parentheses for values, semicolons for termination)

type
  UpfileStr* = distinct string

  StrSlice* = object
    p*: ptr UncheckedArray[char]
    z*: pointer # having the end pointer is more efficient than storing the length as it saves an operation during `inc`


proc len*(x: StrSlice): int {.inline.} =
  cast[int](x.z) - cast[int](x.p)

proc `$`*(x: StrSlice): string =
  result = newString(x.len)
  if x.len() > 0:
    copyMem(addr result[0], x.p, x.len)

proc inc*(x: var StrSlice) {.inline.} =
  x.p = cast[ptr UncheckedArray[char]](addr x.p[1])

proc atEof*(p: StrSlice): bool {.inline.} =
  p.len == 0


proc toSlice*(buff: ptr UncheckedArray[char], len: int): StrSlice {.inline.} =
  StrSlice(p : buff, z : cast[ptr[UncheckedArray[char]]](cast[int](buff) + len))

proc toSlice*(buff: openArray[char]): StrSlice {.inline.} =
  toSlice(cast[ptr UncheckedArray[char]](addr buff[0]), buff.len())

template withSlice*(buff: openArray[char], name: untyped, body: untyped): untyped =
  var name = buff.toSlice()
  body


# Escape sequences: $p for '(', $b for ')', $n for newline, $d for '$'
proc upfileEscape*(x: string): UpfileStr =
  x.multiReplace(
    ("$", "$d"),
    ("(", "$p"),
    (")", "$b"),
    ("\n", "$n")
  ).UpfileStr

proc upfileUnescape*(x: UpfileStr): string =
  x.string.multiReplace(
    ("$d", "$"),
    ("$p", "("),
    ("$b", ")"),
    ("$n", "\n")
  )


proc skipWhitespace*(p: var StrSlice) =
  while not p.atEof() and p.p[0] in {' ', '\n'}:
    inc p

proc expectChar*(p: var StrSlice, c: char) =
  p.skipWhitespace()
  if p.p[0] != c:
    raiseAssert "Expected " & c & " got " & p.p[0]
  inc p

proc peekChar*(p: var StrSlice): char {.inline.} =
  p.skipWhitespace()
  p.p[0]


template withParens*(p: var StrSlice, body: untyped): untyped =
  p.expectChar('(')
  body
  p.expectChar(')')


proc takeAsciiWord*(p: var StrSlice): StrSlice =
  p.skipWhitespace()
  result = StrSlice(p : p.p)
  while p.p[0].isAlphaAscii():
    inc p
  result.z = p.p

proc takeAnyNonTerm*(p: var StrSlice): StrSlice =
  p.skipWhitespace()
  result = StrSlice(p : p.p)
  while p.p[0] notin {' ', '\n'} and p.p[0] notin TermChars:
    inc p
  result.z = p.p

proc takeInt*(p: var StrSlice): int {.inline.} =
  parseInt($p.takeAnyNonTerm())

proc takeString*(p: var StrSlice): string {.inline.} =
  upfileUnescape(UpfileStr($p.takeAnyNonTerm()))

proc takeScope*(p: var StrSlice): StrSlice =
  p.skipWhitespace()
  result = StrSlice(p : p.p)
  p.withParens:
    var depth = 1
    while true:
      if p.p[0] == '(':
        inc depth
      elif p.p[0] == ')':
        dec depth
        if depth == 0:
          break
      inc p
  result.z = p.p

proc takeTaggedValueContent*(p: var StrSlice): StrSlice =
  result = p.takeScope()
  result.p = cast[ptr UncheckedArray[char]](cast[int](result.p) + 1)
  result.z = cast[pointer](cast[int](result.z) - 1)

proc takeTaggedValue*(p: var StrSlice, expectedTag: string): StrSlice =
  p.skipWhitespace()
  var tag = p.takeAsciiWord()
  # Handle tags with digits like i32, i64
  while not p.atEof() and p.p[0] in {'0'..'9'}:
    inc p
  tag.z = p.p
  doAssert $tag == expectedTag, "Expected " & expectedTag & " tag, got '" & $tag & "'"
  p.takeTaggedValueContent()

proc takeTaggedString*(p: var StrSlice): string =
  upfileUnescape(UpfileStr($p.takeTaggedValue("s")))

proc takeTaggedI32*(p: var StrSlice): int32 =
  parseInt($p.takeTaggedValue("i32")).int32

proc takeTaggedI64*(p: var StrSlice): int64 =
  parseInt($p.takeTaggedValue("i64")).int64

template parenLoop*(p: var StrSlice, body: untyped): untyped =
  p.withParens:
    while p.peekChar() != ')':
      body

template fieldLoop*(p: var StrSlice, fieldNameVar: untyped, body: untyped): untyped =
  p.parenLoop:
    let `fieldNameVar` = $p.takeAnyNonTerm()
    body


iterator iterUpfileEntities*(data: openArray[char]): StrSlice =
  if data.len() > 0:
    data.withSlice(p):
      while not p.atEof():
        yield p.takeScope()
        p.skipWhitespace()

proc countUpfileEntities*(data: openArray[char]): int =
  result = 0
  for _ in data.iterUpfileEntities():
    inc result

proc seekNthEntity*(p: var StrSlice, n: int) =
  for i in 0 ..< n:
    discard p.takeScope()

proc takeNthEntityInUpfile*(data: openArray[char], n: int): StrSlice =
  doAssert data.len() > 0
  data.withSlice(p):
    p.seekNthEntity(n)
    p.takeScope()


type
  UpfileWriter* = object
    buff*: string # TODO: Replace with a stream
    pretty*: bool
    indent: int
    afterNewline: bool


proc writeRaw*(p: var UpfileWriter, x: string) =
  if p.pretty and p.afterNewline:
    p.buff.add(repeat("    ", p.indent))
    p.afterNewline = false
  p.buff.add(x)

proc putStr*(p: var UpfileWriter, x: string) =
  p.writeRaw upfileEscape(x).string

proc newline*(p: var UpfileWriter, force: bool = false) =
  if p.pretty or force:
    p.writeRaw("\n")
  p.afterNewline = true

proc terminator*(p: var UpfileWriter) =
  p.writeRaw(";")
  if p.pretty:
    p.newline()

template scope*(p: var UpfileWriter, body: untyped): untyped =
  p.writeRaw("(")
  p.newline()
  inc p.indent
  body
  dec p.indent
  p.writeRaw(")")
  p.newline()

template entity*(p: var UpfileWriter, body: untyped): untyped =
  p.scope:
    body


proc isValidGroupName*(x: string): bool =
  x.allCharsInSet(Letters)

template group*(p: var UpfileWriter, name: string, body: untyped): untyped =
  doAssert name.isValidGroupName()
  p.writeRaw(name)
  p.scope:
    body

template terminated*(p: var UpfileWriter, body: untyped): untyped =
  body
  p.terminator()

proc taggedValue*(x: var UpfileWriter, tag, value: string) =
  x.writeRaw tag & "(" & value & ")"

template taggedField*(x: var UpfileWriter, name: string, body: untyped): untyped =
  doAssert name.isValidGroupName(), "Names must not contain spaces: '" & name & "'"
  x.terminated:
    x.writeRaw name & " "
    body

proc fieldStr*(x: var UpfileWriter, name, value: string) =
  x.taggedField(name):
    x.taggedValue("s", upfileEscape(value).string)

proc fieldI32*(x: var UpfileWriter, name: string, value: int32) =
  x.taggedField(name):
    x.taggedValue("i32", $value)

proc fieldI64*(x: var UpfileWriter, name: string, value: int64) =
  x.taggedField(name):
    x.taggedValue("i64", $value)


when isMainModule:
  import std/[
    times,
  ]

  var data = block:
    var x = UpfileWriter(buff : newString(0), pretty : false)
    x.entity:
      x.group("test"):
        x.fieldI32("number", 10)
        x.fieldStr("string", "some (text) $ for testing;")
    x.buff
  while data.len() < 1_000_000_000:
    data &= data

  echo "Data size: ", data.len(), " | ", data.len().formatSize()
  let count = countUpfileEntities(data)
  echo "Entity count: ", count

  let parseStart = cpuTime()
  let x = takeNthEntityInUpfile(data, count - 1)
  echo x

  #echo x.raw
  echo "Took ", cpuTime() - parseStart
  #echo x.len()
  echo getOccupiedMem().formatSize()
