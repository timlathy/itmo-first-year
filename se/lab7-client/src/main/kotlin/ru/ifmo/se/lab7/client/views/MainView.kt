package ru.ifmo.se.lab7.client.views

import ru.ifmo.se.lab7.client.components.NavigationHeader
import ru.ifmo.se.lab7.client.controllers.MainController
import tornadofx.*
import ru.ifmo.se.lab7.client.controllers.EmploymentRequestController

class MainView : View("EmploymentRequest Manager") {
  val mainController: MainController by inject()
  val dataController: EmploymentRequestController by inject()

  val objectView: EmploymentRequestView by inject()

  private val views = mapOf("Map" to MapView(), "Dashboard" to DashboardView())
  private val navigation = NavigationHeader(views["Map"]!!, views)

  override val root = vbox {
    styleClass.add("content-root")
    spacing = 6.0

    add(navigation)
    add(views["Map"]!!)
  }

  init {
    runAsync { dataController.refreshObjectList() }
    subscribe<MapView.PinSelectionEvent> { e ->
      objectView.model.rebind { employmentRequest = e.element }
      navigation.navigateTo(objectView)
    }
  }
}
