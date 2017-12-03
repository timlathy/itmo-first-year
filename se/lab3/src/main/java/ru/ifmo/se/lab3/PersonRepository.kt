package ru.ifmo.se.lab3

import org.springframework.data.repository.CrudRepository

interface PersonRepository : CrudRepository<Person, Long> {
  fun findByName(name: String): Person
}
