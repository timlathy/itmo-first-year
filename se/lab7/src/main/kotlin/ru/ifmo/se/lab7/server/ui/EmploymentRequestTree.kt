package ru.ifmo.se.lab7.server.ui

import ru.ifmo.se.lab7.server.CollectionChange
import ru.ifmo.se.lab7.server.EmploymentRequest
import java.util.*
import javax.swing.JTree
import javax.swing.event.EventListenerList
import javax.swing.tree.DefaultMutableTreeNode
import javax.swing.tree.DefaultTreeModel
import javax.swing.tree.TreeSelectionModel

class EmploymentRequestTree: JTree(rootNode) {
  companion object {
    val rootNode = DefaultMutableTreeNode().apply {
      EmploymentRequest.Status.values().forEach { add(DefaultMutableTreeNode(it)) }
    }

    /* Tree depth of meta nodes (root and status descriptions) */
    const val metaDepth = 2
  }

  //<editor-fold defaultstate="collapsed" desc="Events">
  class ElementSelectionChangeEvent(source: Any, val selected: EmploymentRequest?): EventObject(source)

  interface ElementSelectionChangeEventListener: EventListener {
    fun onElementSelectionChanged(e: ElementSelectionChangeEvent)
  }

  val eventListeners = EventListenerList()

  fun addElementSelectionChangeListener(listener: ElementSelectionChangeEventListener) =
    eventListeners.add(ElementSelectionChangeEventListener::class.java, listener)

  private fun triggerElementSelectionChangeEvent(e: ElementSelectionChangeEvent) =
    eventListeners.getListeners(ElementSelectionChangeEventListener::class.java).forEach { it.onElementSelectionChanged(e) }
  //</editor-fold>

  init {
    selectionModel.selectionMode = TreeSelectionModel.SINGLE_TREE_SELECTION

    addTreeSelectionListener {
      val selected = if (selectionPath != null && selectionPath.pathCount > metaDepth)
        (lastSelectedPathComponent as DefaultMutableTreeNode?)?.userObject as EmploymentRequest
      else null

      triggerElementSelectionChangeEvent(ElementSelectionChangeEvent(it, selected))
    }
  }

  override fun getModel(): DefaultTreeModel? = super.getModel()?.let { it as DefaultTreeModel }

  fun updateWithChanges(changes: List<CollectionChange<EmploymentRequest>>) {
    changes.forEach { (element, status) ->
      val statusRoot = rootNode.getChildAt(element.status.ordinal) as DefaultMutableTreeNode
      when (status) {
        CollectionChange.ChangeType.ADDITION ->
          model?.insertNodeInto(DefaultMutableTreeNode(element), statusRoot, statusRoot.childCount)
        CollectionChange.ChangeType.REMOVAL ->
          statusRoot.depthFirstEnumeration().iterator().asSequence()
            .find { (it as DefaultMutableTreeNode).userObject == element }
            ?.let { model?.removeNodeFromParent(it as DefaultMutableTreeNode) }
      }
    }
  }
}
