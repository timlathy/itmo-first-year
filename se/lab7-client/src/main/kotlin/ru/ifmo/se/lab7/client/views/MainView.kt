package ru.ifmo.se.lab7.client.views

import ru.ifmo.se.lab7.client.components.NavigationHeader
import ru.ifmo.se.lab7.client.controllers.MainController
import tornadofx.*
import ru.ifmo.se.lab7.client.controllers.EmploymentRequestController
import ru.ifmo.se.lab7.client.models.EmploymentRequest

class MainView : View("EmploymentRequest Manager") {
  val mainController: MainController by inject()
  val dataController: EmploymentRequestController by inject()

  private val objectView: EmploymentRequestView by inject()
  private val filterView: FilterView by inject()

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

    subscribe<NavigationHeader.FilterRequest> { navigation.navigateTo(filterView) }

    subscribe<NavigationHeader.FilterPredicateApplied> { e ->
      dataController.setObjectListPredicate(e.predicate)
    }

    subscribe<NavigationHeader.NewItemRequest> { openEditor() }

    subscribe<NavigationHeader.RefreshRequest> {
      runAsync {
        dataController.refreshObjectList()
      } ui {
        navigation.onRefreshCompleted()
      }
    }

    subscribe<MapView.PinSelectionEvent> { e -> openEditor(e.element) }

    subscribe<EmploymentRequestView.ObjectActionRequest> { e ->
      navigation.navigateBack()
      runAsync {
        dataController.executeAction(e.action, e.element, e.auxElement)
      } ui {
        println(it)
        navigation.forceRefreshAction()
      }
    }
  }

  private fun openEditor(target: EmploymentRequest? = null) {
    objectView.model.rebind { employmentRequest = target ?: EmploymentRequest() }
    objectView.isNewModel.set(target == null)
    navigation.navigateTo(objectView)
  }
}
