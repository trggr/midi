drop view bar_v;

create view bar_v
as
select b.*, c.root_midi_num, c.major_ind
from bar             b
     left join chord c on (c.chord_id = b.chord_id);
