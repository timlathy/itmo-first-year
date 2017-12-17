package ru.ifmo.se.lab3.repository

import org.springframework.data.repository.CrudRepository
import org.springframework.data.repository.NoRepositoryBean 

import ru.ifmo.se.lab3.domain.Action
import ru.ifmo.se.lab3.domain.Person

@NoRepositoryBean
interface AbstractActionRepository<T> : CrudRepository<T, Long> {
  fun findByActor(actor: Person): Iterable<T>
  fun findByActorName(name: String): Iterable<T>
}

interface ActionRepository : AbstractActionRepository<Action>
