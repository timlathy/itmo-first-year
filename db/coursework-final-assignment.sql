-- Between genders, which one is more likely to have people _from the same club_:
-- * be older than 16
-- * have more than 2 event participations
-- * have more event participations and better exam marks than the club president
-- * have sent at least one parcel with delivery time exceeding 5 days

create or replace view parcel_senders as
  select sp.id from people p
  inner join student_profiles sp on p.id = sp.person_id
  inner join delivery_owl_flights dof on p.id = dof.sender_id
    and dof.contents_type = 'parcel'
    and (returned_on is null and departed_on < current_date - interval '5 days'
         or returned_on is not null and (returned_on - departed_on) >= interval '5 days');

create or replace view frequent_participators as
  select sp.id as profile_id, count(ep) as count from people p
  inner join student_profiles sp on p.id = sp.person_id
  inner join event_participations ep on sp.id = ep.student_profile_id
  group by sp.id having count(ep) > 2;

create or replace view club_gender_stats as
  select sc.id as club_id, p.gender, avg(case er.mark when 'passed' then 5 else 2 end) as perf, avg(fp.count) as pcs from people p
  inner join student_profiles sp on p.id = sp.person_id
  inner join exam_results er on sp.id = er.student_profile_id
  inner join event_participations ep on sp.id = ep.student_profile_id
  inner join student_clubs sc on sp.club_id = sc.id
  inner join frequent_participators fp on sp.id = fp.profile_id
  group by sc.id, p.gender;

create or replace view club_president_performance as
  select sc.id as club_id, avg(case er.mark when 'passed' then 5 else 2 end) as perf, avg(fp.count) as pcs from people p
  inner join student_profiles sp on p.id = sp.person_id
  inner join student_clubs sc on sp.id = sc.president_id
  inner join exam_results er on sp.id = er.student_profile_id
  inner join event_participations ep on sp.id = ep.student_profile_id
  inner join frequent_participators fp on sp.id = fp.profile_id
  group by sc.id;

select p.gender, count(sc.id) from people p
  inner join student_profiles sp on p.id = sp.person_id
    and sp.id in (select * from parcel_senders)
    and sp.id in (select profile_id from frequent_participators)
  inner join student_clubs sc on sp.club_id = sc.id
  inner join club_gender_stats cgs on sc.id = cgs.club_id
  inner join club_president_performance cpp on sc.id = cpp.club_id and cpp.perf < cgs.perf and cpp.pcs < cgs.pcs
  where p.birth_date < (current_date - interval '16 years')
  group by p.gender
  order by 2 desc;