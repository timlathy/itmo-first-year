package ru.ifmo.se.lab3

import org.springframework.data.repository.CrudRepository
import org.springframework.data.repository.NoRepositoryBean 

@NoRepositoryBean
interface AbstractActionRepository<T> : CrudRepository<T, Long> {
  fun findByActor(actor: Person): Iterable<T>
}

interface ActionRepository : AbstractActionRepository<Action>
