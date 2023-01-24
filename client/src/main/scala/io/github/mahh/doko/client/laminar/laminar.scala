package io.github.mahh.doko.client.laminar

import com.raquo.laminar.api.L
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes
import org.scalajs.dom

@inline def renderOnDomContentLoaded(
  selectors: => String,
  rootNode: => nodes.ReactiveElement.Base
): Unit = L.renderOnDomContentLoaded(
  dom.document.querySelector(selectors),
  rootNode
)
