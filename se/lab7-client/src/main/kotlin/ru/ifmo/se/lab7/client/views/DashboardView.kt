package ru.ifmo.se.lab7.client.views

import tornadofx.*

class DashboardView: View() {
  override val root = hbox {
    label("Coming soon, a unified place to manage the flow of your employment requests, assess progress, and overcome deadlines. " +
      "Follow application updates to receive the new version as soon as possible!").apply {
      style = "-fx-font-size: 16px; -fx-padding: 100;"
      isWrapText = true
    }
  }
}
