package io.github.mahh.doko.client.components

import com.raquo.laminar.api.L.*

private def stringsWithLineBreaks(strings: Signal[List[String]]): Signal[List[Node]] =
  strings.map(_.flatMap(s => List(textToNode(s), br())))
