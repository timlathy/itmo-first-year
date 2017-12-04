package ru.ifmo.se.lab3

import org.springframework.data.repository.CrudRepository

interface BusinessRepository : CrudRepository<Business, Long> {
  fun findByName(name: String): Business
  fun findByOwner(owner: Person): Iterable<Business>
}
