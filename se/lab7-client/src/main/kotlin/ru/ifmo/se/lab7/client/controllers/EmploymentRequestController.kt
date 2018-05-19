package ru.ifmo.se.lab7.client.controllers

import ru.ifmo.se.lab7.client.models.EmploymentRequest
import ru.ifmo.se.lab7.client.models.Location
import tornadofx.*

class EmploymentRequestController: Controller() {
  private var cachedObjects: List<EmploymentRequest>? = null

  fun getObjects(forceRefresh: Boolean = false): List<EmploymentRequest> {
    if (cachedObjects == null || forceRefresh) updateObjects()
    return cachedObjects as List<EmploymentRequest>
  }

  private fun updateObjects() {
    cachedObjects = listOf(
      EmploymentRequest("Amy J", location = Location(112.0, 432.3)),
      EmploymentRequest("John A", location = Location(240.0, 230.3), status = EmploymentRequest.Status.REJECTED))
  }
}
