package ru.ifmo.se.lab7.client.views

import javafx.scene.Node
import javafx.scene.control.Button
import javafx.scene.text.Text
import ru.ifmo.se.lab7.client.components.PannableCanvas
import ru.ifmo.se.lab7.client.controllers.EmploymentRequestController
import ru.ifmo.se.lab7.client.models.EmploymentRequest
import ru.ifmo.se.lab7.client.models.EmploymentRequest.Status.*
import tornadofx.*
import java.lang.ref.WeakReference

class MapView: View() {
  private val controller: EmploymentRequestController by inject()

  private val statusGlyphs: Map<EmploymentRequest.Status, String> =
    mapOf(PROCESSING to "\uf017", INTERVIEW_SCHEDULED to "\uf058", REJECTED to "\uf057")

  /* A lookup structure to enable faster node removal on data changes. */
  private val mapNodes: MutableMap<EmploymentRequest, WeakReference<Node>> = mutableMapOf()

  private val map = pane {
    styleClass.add("map-pane")

    prefWidth = 2000.0
    prefHeight = 2000.0
  }

  override val root = vbox {
    add(PannableCanvas(map))
  }

  class PinSelectionEvent(val element: EmploymentRequest): FXEvent()

  init {
    controller.objectList.onChange { change ->
      runLater { with(change) { while (next()) {
        removed.forEach {
          mapNodes[it]?.get()?.removeFromParent()
          mapNodes.remove(it)
        }
        addedSubList.forEach {
          val mapMarker: Node = MapMarker(it).apply {
            action { this@MapView.fire(PinSelectionEvent(element)) }
          }
          mapNodes[it] = WeakReference(mapMarker)
          map.add(mapMarker)
        }
      }}}
    }
  }

  inner class MapMarker(val element: EmploymentRequest): Button("\uf3c5") {
    init {
      styleClass.addAll("map-pin", "map-pin--orange")

      layoutX = element.location.first
      layoutY = element.location.second

      tooltip {
        text = element.applicant

        styleClass.add("map-pin__tooltip")
        graphic = Text(statusGlyphs[element.status]).apply {
          styleClass.add("map-pin__tooltip-icon")
        }

        graphicTextGap = 8.0
      }
    }
  }
}
