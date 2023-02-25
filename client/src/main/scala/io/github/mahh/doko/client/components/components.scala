package io.github.mahh.doko.client.components

import com.raquo.laminar.api.L.*

private def stringsWithLineBreaks(strings: Signal[List[String]]): Signal[List[Node]] =
  strings.map(_.flatMap(s => List(textToNode(s), br())))

// unicode zero-width empty space - appears to be the easiest way to enforce regular string
// height for an empty string
private val fullHeightEmptyString = '\u200b'.toString
