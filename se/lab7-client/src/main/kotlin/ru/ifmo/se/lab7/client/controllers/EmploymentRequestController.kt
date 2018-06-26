package ru.ifmo.se.lab7.client.controllers

import ru.ifmo.se.lab7.client.models.EmploymentRequest
import tornadofx.*
import java.io.StringReader
import javax.json.Json

class EmploymentRequestController: Controller() {
  val api: Rest by inject()

  init {
    Rest.useApacheHttpClient()
    api.baseURI = "http://localhost:8080"
  }

  var objectList = SortedFilteredList<EmploymentRequest>(observableList())
    private set

  enum class Actions {
    ADD, ADD_IF_HIGHEST_PRIORITY, ADD_IF_LOWEST_PRIORITY,
    REMOVE, REMOVE_ALL, REMOVE_ALL_HIGHER_PRIORITY, REMOVE_ALL_LOWER_PRIORITY,
    EDIT
  }

  fun handleError(resp: Rest.Response): String? = when (resp.statusCode) {
    200, 201 -> null
    422 -> "api.validation_error"
    else -> "api.error"
  }

  fun executeAction(action: Actions, element: EmploymentRequest, auxElement: EmploymentRequest?) =
    try {
      when (action) {
        Actions.ADD -> api.post("/queue", element)
        Actions.ADD_IF_HIGHEST_PRIORITY -> api.post("/queue?mode=if_max", element)
        Actions.ADD_IF_LOWEST_PRIORITY -> api.post("/queue?mode=if_min", element)
        Actions.REMOVE -> api.delete("/queue/${element.id}")
        Actions.REMOVE_ALL -> api.delete("/queue")
        Actions.REMOVE_ALL_HIGHER_PRIORITY -> api.delete("/queue?mode=greater", element)
        Actions.REMOVE_ALL_LOWER_PRIORITY -> api.delete("/queue?mode=lesser", element)
        Actions.EDIT -> api.patch("/queue/${element.id}", auxElement!!)
      }.let(::handleError)
    }
    catch (e: Exception) { "api.error" }

  fun addAllSerialized(jsonItems: String) {
    jsonItems
      .let { Json.createReader(StringReader(it)) }
      .readArray()
      .toModel<EmploymentRequest>()
      .forEach { executeAction(Actions.ADD, it, null) }
  }

  fun setObjectListPredicate(pred: (EmploymentRequest) -> Boolean) {
    objectList.predicate = pred
  }

  fun refreshObjectList() =
    try {
      api.get("/queue").list().toModel<EmploymentRequest>().let { objectList.items.setAll(it) }
      null
    }
    catch (e: Exception) { "api.error" }
}
