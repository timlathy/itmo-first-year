-- Task 1

create temp view romantic_relationships as
  select id, array_agg(member_id order by member_id) as partner_ids from relationships
  inner join crew_members_relationships on id = relationship_id
  where kind = 'romance'
  group by id;

create or replace function duo_observations(used_tool_name text)
  returns table (observer_ids integer[]) as
  $$
    select array_agg(observer_id order by observer_id) from scientific_observations
      where equipment_id = (select id from observation_tools where name = $1)
      group by observed_on having count(*) = 2
  $$
  language sql;

select id relationship_id, partner_ids
  from romantic_relationships
  where partner_ids in (select duo_observations('Telescope'));

-- Task 2

create or replace function observation_log(starting_from_id integer)
  returns setof text as
  $$
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