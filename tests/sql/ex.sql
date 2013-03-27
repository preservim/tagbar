select user_id, count(*) as how_many
from bboard
where not exists (select 1 from 
                  bboard_authorized_maintainers bam
                  where bam.user_id = bboard.user_id)
and posting_time + 60 > sysdate
group by user_id
order by how_many desc;

select user_id, count(*) as how_many
from bboard
where posting_time + 60 > sysdate
group by user_id
having not exists (select 1 from 
                  bboard_authorized_maintainers bam
                  where bam.user_id = bboard.user_id)
order by how_many desc;
