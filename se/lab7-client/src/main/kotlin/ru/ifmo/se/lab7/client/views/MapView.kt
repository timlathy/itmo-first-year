package ru.ifmo.se.lab7.client.views

import javafx.scene.image.Image
import javafx.scene.image.ImageView
import javafx.scene.layout.Background
import javafx.scene.layout.BackgroundFill
import javafx.scene.paint.Color
import ru.ifmo.se.lab7.client.components.PannableCanvas
import ru.ifmo.se.lab7.client.controllers.EmploymentRequestController
import ru.ifmo.se.lab7.client.models.EmploymentRequest
import ru.ifmo.se.lab7.client.models.EmploymentRequest.Status.*
import tornadofx.*

class MapView: View() {
  private val controller: EmploymentRequestController by inject()

  private val statusIcons: Map<EmploymentRequest.Status, Image> by lazy { mapOf(
    PROCESSING          to Image(resources["/faicons/clock-white.png"]),
    INTERVIEW_SCHEDULED to Image(resources["/faicons/check-white.png"]),
    REJECTED            to Image(resources["/faicons/ban-white.png"]))
  }

  private val map = pane {
    prefWidth = 2000.0
    prefHeight = 2000.0
    background = Background(BackgroundFill(Color.WHITE, null, null))
  }

  override val root = vbox {
    add(PannableCanvas(map))
  }

  fun displayItems(refresh: Boolean = false) {
    map.children.clear()
    controller.getObjects(refresh).forEach {
      map.add((button {
        styleClass.addAll("map-pin", "map-pin--orange")

        layoutX = it.location.first
        layoutY = it.location.second

        tooltip {
          text = it.applicant

          graphic = ImageView(statusIcons[it.status]).apply {
            fitHeight = 18.0
            fitWidth = 18.0
          }

          graphicTextGap = 8.0
        }
      }))
    }
  }
}
