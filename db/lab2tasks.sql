-- Task 1

with romantic_relationships as (
  select r1.relationship_id, min(r1.member_id) as partner_a_id, max(r2.member_id) as partner_b_id
  from crew_members_relationships r1
  inner join crew_members_relationships r2
    on r1.relationship_id = r2.relationship_id and r1.member_id != r2.member_id
  inner join relationships r
    on r1.relationship_id = r.id and r.kind = 'romance'
  group by r1.relationship_id
),
duo_telescope_observations as (
  select min(o1.observer_id) as partner_a_id, max(o2.observer_id) as partner_b_id
  from scientific_observations o1
  inner join scientific_observations o2
    on o1.observed_on = o2.observed_on
       and o1.observer_id != o2.observer_id
       and o1.equipment_id = o2.equipment_id
  where o1.equipment_id = (select id from observation_tools where name = 'Telescope')
  group by o1.observed_on
)
select r.relationship_id, r.partner_a_id, r.partner_b_id
from romantic_relationships r
  inner join duo_telescope_observations o
    on o.partner_a_id = r.partner_a_id and o.partner_b_id = r.partner_b_id;

-- (Generic version that works for relationships of > 2 people)

with romantic_relationships as (
  select id, array_agg(member_id order by member_id) as partner_ids
  from relationships
  inner join crew_members_relationships on id = relationship_id
  where kind = 'romance'
  group by id
),
duo_telescope_observations as (
  select array_agg(observer_id order by observer_id)
  from scientific_observations
  where equipment_id = (select id from observation_tools where name = 'Telescope')
  group by observed_on
  having count(*) = 2
)
select id relationship_id, partner_ids
from romantic_relationships
where partner_ids in (select * from duo_telescope_observations);

-- Task 2

create or replace function observation_log(starting_from_id integer)
  returns setof text as $$
    declare
      current_item record;
    begin
      select into current_item * from scientific_observations
        where id = starting_from_id;

      loop
        return next format(
            '%s - %s - %s - %s',
            (select full_name from crew_members where id = current_item.observer_id),
            (select name from observation_tools where id = current_item.equipment_id),
            current_item.details,
            current_item.observed_on);

        select into current_item * from scientific_observations
          where base_observation_id = current_item.id;

        if current_item is null then
          return;
        end if;
      end loop;
    end;
  $$
  language plpgsql;
select observation_log(1);
