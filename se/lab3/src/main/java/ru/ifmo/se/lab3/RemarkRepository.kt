package ru.ifmo.se.lab3

import org.springframework.data.repository.CrudRepository
import org.springframework.data.repository.NoRepositoryBean 

@NoRepositoryBean
interface AbstractRemarkRepository<T> : CrudRepository<T, Long> {
  fun findBySpeaker(speaker: Character): Iterable<T>
}

interface RemarkRepository : AbstractRemarkRepository<Remark>
