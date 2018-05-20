package ru.ifmo.se.lab7.client.controllers

import ru.ifmo.se.lab7.client.models.EmploymentRequest
import ru.ifmo.se.lab7.client.models.Location
import tornadofx.*

class EmploymentRequestController: Controller() {
  var objectList = SortedFilteredList<EmploymentRequest>(observableList())
    private set

  fun refreshObjectList() {
    objectList.items.addAll(
      EmploymentRequest("Amy J", location = Location(112.0, 432.3)),
      EmploymentRequest("John A", location = Location(240.0, 230.3), status = EmploymentRequest.Status.REJECTED),
      EmploymentRequest("K A", location = Location(390.0, 540.3), status = EmploymentRequest.Status.INTERVIEW_SCHEDULED))
  }
}
