package ru.ifmo.se.lab7.client.views

import javafx.animation.*
import javafx.scene.Node
import javafx.scene.control.Button
import javafx.scene.text.Text
import javafx.util.Duration
import ru.ifmo.se.lab7.client.components.PannableCanvas
import ru.ifmo.se.lab7.client.controllers.EmploymentRequestController
import ru.ifmo.se.lab7.client.models.EmploymentRequest
import ru.ifmo.se.lab7.client.models.EmploymentRequest.Status.*
import ru.ifmo.se.lab7.client.models.EmploymentRequest.ColorCode.*
import tornadofx.*
import java.lang.ref.WeakReference

class MapView: View() {
  companion object {
    const val PARTY_TIME_OFFSET = 100
    const val PARTY_TIME_FORWARD_TIME = 5_000.0
    const val PARTY_TIME_BACKWARD_TIME = 2_000.0

    const val MAX_LATITUDE = 90.0
    const val MAX_LONGITUDE = 180.0
    const val MAP_AXIS_SCALE = 10.0
  }

  private val controller: EmploymentRequestController by inject()

  private val statusGlyphs: Map<EmploymentRequest.Status, String> =
    mapOf(PROCESSING to "\uf017", INTERVIEW_SCHEDULED to "\uf058", REJECTED to "\uf057")

  /* A lookup structure to enable faster node removal on data changes. */
  private val mapNodes: MutableMap<EmploymentRequest, WeakReference<Node>> = mutableMapOf()

  private val map = pane {
    styleClass.add("map-pane")

    prefWidth = MAX_LONGITUDE * MAP_AXIS_SCALE
    prefHeight = MAX_LATITUDE * MAP_AXIS_SCALE + /* dirty fix for height shrinking */ 2_000
  }

  override val root = vbox {
    add(PannableCanvas(map))
  }

  private var partyTimeline: Timeline? = null

  fun createAndStartPartyTimeline() {
    partyTimeline = Timeline().apply {
      setOnFinished {
        fire(PartyTimeOverEvent())
      }

      mapNodes.values
        .mapNotNull(WeakReference<Node>::get)
        .forEach {
          val xMovement = Math.min(it.layoutX + PARTY_TIME_OFFSET, map.width)
          val forwardTransition = KeyValue(it.layoutXProperty(), xMovement, Interpolator.EASE_BOTH)
          val backwardTransition = KeyValue(it.layoutXProperty(), it.layoutX, Interpolator.EASE_BOTH)

          keyFrames.add(KeyFrame(Duration(PARTY_TIME_FORWARD_TIME), forwardTransition))
          keyFrames.add(KeyFrame(Duration(PARTY_TIME_FORWARD_TIME + PARTY_TIME_BACKWARD_TIME), backwardTransition))
        }
    }
  }

  fun setPartyTime(enable: Boolean = true) {
    if (enable) {
      createAndStartPartyTimeline()
      partyTimeline?.play()
    }
    else if (partyTimeline?.status == Animation.Status.RUNNING) {
      partyTimeline?.jumpTo(Duration(PARTY_TIME_FORWARD_TIME + PARTY_TIME_BACKWARD_TIME))
      partyTimeline?.stop()
    }
  }

  class PartyTimeOverEvent: FXEvent()

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
      styleClass.addAll("map-pin", when (element.colorCode) {
        BLUE  -> "map-pin--blue"
        GREEN -> "map-pin--green"
        else  -> "map-pin--orange"
      })

      layoutX = element.locLongitude * MAP_AXIS_SCALE
      layoutY = element.locLatitude * MAP_AXIS_SCALE

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
