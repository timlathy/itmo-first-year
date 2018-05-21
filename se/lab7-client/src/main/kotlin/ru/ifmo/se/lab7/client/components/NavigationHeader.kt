package ru.ifmo.se.lab7.client.components

import javafx.animation.Interpolator
import javafx.animation.RotateTransition
import javafx.scene.control.Button
import javafx.scene.layout.Priority
import javafx.util.Duration
import tornadofx.*

class NavigationHeader(expandedView: View, navigableViews: Map<String, View>): Fragment() {
  private var topLevelView = expandedView

  private val nestedNavigationPath = mutableListOf<View>()

  private val topLevelLinks: List<Button> = navigableViews.map {
    button(it.key.toUpperCase()) {
      styleClass.add("navigation-header__item")

      if (topLevelView == it.value) isDisable = true

      action {
        if (topLevelView != it.value) {
          resetTopLevelLinks()
          isDisable = true

          topLevelView.replaceWith(it.value)
          topLevelView = it.value
        }
      }
    }
  }

  //<editor-fold defaultstate="collapsed" desc="Top-level controls (refresh, new item)">
  private val refreshButton: Button = button("\uf2f1") {
    styleClass.add("navigation-header__control")

    action {
      refreshInProgress = !refreshInProgress
      if (refreshInProgress) {
        refreshAnimation.play()
        fire(RefreshRequest())
      }
    }
  }

  private val newItemButton: Button = button("\uf303") {
    styleClass.add("navigation-header__control")

    action { fire(NewItemRequest()) }
  }

  private val topLevelControls: List<Button> = listOf(refreshButton, newItemButton)

  class NewItemRequest: FXEvent()

  class RefreshRequest: FXEvent()

  private val refreshAnimation = RotateTransition(Duration.millis(1000.0), refreshButton).apply {
    byAngle = 360.0
    interpolator = Interpolator.LINEAR
  }

  init { refreshAnimation.setOnFinished { if (refreshInProgress) refreshAnimation.play() } }

  private var refreshInProgress = false

  fun onRefreshCompleted() { refreshInProgress = false }
  //</editor-fold>

  private val topLevelBox = hbox {
    hgrow = Priority.ALWAYS

    topLevelLinks.forEach { add(it) }

    pane { hgrow = Priority.ALWAYS }

    topLevelControls.forEach { add(it) }
  }

  private val nestedNavigationBox = hbox {
    button("\uf060") {
      styleClass.add("navigation-header__control")
      action(::popSubview)
    }
  }

  override val root = hbox {
    styleClass.add("navigation-header")
    add(topLevelBox)
  }

  fun navigateTo(subview: View) {
    if (nestedNavigationPath.isEmpty()) {
      topLevelBox.removeFromParent()
      root.add(nestedNavigationBox)
    }
    nestedNavigationPath.add(subview)
    topLevelView.replaceWith(subview)
  }

  private fun popSubview() {
    val popped = nestedNavigationPath.last()
    nestedNavigationPath.removeAt(nestedNavigationPath.lastIndex)
    if (nestedNavigationPath.isEmpty()) {
      nestedNavigationBox.removeFromParent()
      root.add(topLevelBox)
      popped.replaceWith(topLevelView)
    }
    else popped.replaceWith(nestedNavigationPath.last())
  }

  private fun resetTopLevelLinks() = topLevelLinks.forEach { it.isDisable = false }
}
