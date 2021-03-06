package ru.ifmo.se.lab7.client.components

import javafx.geometry.Insets
import javafx.geometry.Pos
import javafx.scene.Group
import javafx.scene.Node
import javafx.scene.control.ScrollPane
import javafx.scene.layout.VBox
import tornadofx.*

class PannableCanvas(content: Node): Fragment() {
  override val root = anchorpane {
    val zoomPane = ZoomableScrollPane(content).apply {
      styleClass.add("pannable-canvas__pane")
      anchorpaneConstraints {
        topAnchor = 0.0; rightAnchor = 0.0; bottomAnchor = 0.0; leftAnchor = 0.0
      }
    }

    add(zoomPane)
    add(vbox {
      maxHeight = 200.0
      maxWidth = 200.0
      spacing = 12.0
      padding = Insets(6.0)

      anchorpaneConstraints {
        bottomAnchor = 10.0; rightAnchor = 10.0
      }

      button("\uf067") {
        styleClass.add("pannable-canvas__zoom-control")
        action { zoomPane.zoom() }
      }
      button("\uf068") {
        styleClass.add("pannable-canvas__zoom-control")
        action { zoomPane.zoom(out = true) }
      }
    })
  }

  class ZoomableScrollPane(private val target: Node): ScrollPane() {
    companion object {
      const val zoomBy = 0.2
    }

    private var scaleValue = 0.7

    init {
      content = VBox(Group(target)).apply {
        alignment = Pos.CENTER
      }

      hbarPolicy = ScrollPane.ScrollBarPolicy.NEVER
      vbarPolicy = ScrollPane.ScrollBarPolicy.NEVER
      isPannable = true
      isFitToWidth = true
      isFitToHeight = true

      updateScale()
    }

    fun zoom(out: Boolean = false) {
      scaleValue = scaleValue * (if (out) 1 - zoomBy else 1 + zoomBy)
      updateScale()
    }

    private fun updateScale() {
      target.scaleX = scaleValue
      target.scaleY = scaleValue
    }
  }
}
