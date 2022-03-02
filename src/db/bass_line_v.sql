drop view bass_line_v;

create view bass_line_v as
with 
t as (select p.*, c.root_midi_num, c.chord_form_cd, n.midi_num, h.bar_cnt, c.major_ind
      from bass_line_chord     p
           join bass_line      h on (h.bass_line_id = p.bass_line_id)
           join chord          c on (c.chord_id     = p.chord_id)
           cross join note     n
      where n.note_cd in ('c', 'c#', 'd', 'd#', 'e', 'f', 'f#', 'g', 'g#', 'a', 'a#', 'b'))
select t.bass_line_id, 
       t.bass_line_id || '-' || c.chord_id xbass_line_id,
       c.root_midi_num
          - t.root_midi_num
          + case when c.root_midi_num >= t.root_midi_num then 0 else 12 end transp_num,
       t.bar_id, t.beat_id, c.chord_id, t.bar_cnt, t.major_ind,
       t.root_midi_num ptrn_root_midi_num,
       c.root_midi_num transp_root_midi_num
from t
   left join chord c on (c.root_midi_num = t.midi_num and
                         c.chord_form_cd = t.chord_form_cd);
                         

