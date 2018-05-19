package ru.ifmo.se.lab7.client.views

import javafx.scene.Parent
import ru.ifmo.se.lab7.client.models.EmploymentRequest
import tornadofx.*

class ObjectView(private val model: EmploymentRequest) : View("${model.applicant}'s request") {
  override val root = vbox {
    
  }
}
