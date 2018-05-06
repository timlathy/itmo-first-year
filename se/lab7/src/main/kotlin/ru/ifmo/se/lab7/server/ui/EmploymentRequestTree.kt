package ru.ifmo.se.lab7.server.ui

import ru.ifmo.se.lab7.server.CollectionChange
import ru.ifmo.se.lab7.server.EmploymentRequest
import javax.swing.JTree
import javax.swing.tree.DefaultMutableTreeNode
import javax.swing.tree.DefaultTreeModel

class EmploymentRequestTree: JTree(rootNode) {
  companion object {
    val rootNode = DefaultMutableTreeNode().apply {
      EmploymentRequest.Status.values().forEach { add(DefaultMutableTreeNode(it)) }
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
