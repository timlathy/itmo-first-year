package ru.ifmo.se.lab3.repository

import org.springframework.data.repository.CrudRepository

import ru.ifmo.se.lab3.domain.Business
import ru.ifmo.se.lab3.domain.Person

interface BusinessRepository : CrudRepository<Business, Long> {
  fun findByName(name: String): Business
  fun findByOwner(owner: Person): Iterable<Business>
}
