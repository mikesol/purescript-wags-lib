from mido import MidiFile

my_mid = MidiFile('bwv849.mid')

def get_longest_track(mid):
    """
    Get the longest track in a midi file
    """
    longest_track = 0
    ix = 0
    for i, track in enumerate(mid.tracks):
        if len(track) > longest_track:
            longest_track = len(track)
            ix = i
    return longest_track, ix

class NoteCandidate:
  def __init__(self, note, channel, velocities, time_on):
    self.note = note
    self.channel = channel
    self.velocities = velocities
    self.time_on = time_on
  def toNote(self, time_off):
    return Note(self.note, self.channel, self.velocities, self.time_on, time_off)

class Note:
  def __init__(self, note, channel, velocities, time_on, time_off):
    self.note = note
    self.channel = channel
    self.velocities = velocities
    self.time_on = time_on
    self.time_off = time_off

def make_score(mid):
  _, ix = get_longest_track(mid)
  channels = [[[] for y in range(128)] for x in range(16)]
  for i, track in enumerate(mid.tracks):
      if i != ix: continue
      tm = 0
      for j, msg in enumerate(track):
        tm = tm + msg.time
        if msg.type == 'note_on':
          ch = msg.channel
          if channels[ch][msg.note] != [] and not isinstance(channels[ch][msg.note][-1], Note):
            channels[ch][msg.note][-1] = channels[ch][msg.note][-1].toNote(None)
          channels[ch][msg.note] += [NoteCandidate(msg.note, ch, [(tm, msg.velocity)], tm)]
        if msg.type == 'note_off':
          ch = msg.channel
          if not isinstance(channels[ch][msg.note][-1], NoteCandidate):
            raise ValueError('dangling note')
          channels[ch][msg.note][-1] = channels[ch][msg.note][-1].toNote(tm)
      break
  return channels

if __name__ == "__main__":
  chn = make_score(my_mid)
  nts = sorted(sum(sum(chn,[]),[]), key=lambda x: x.time_on)
  #print(nts)
  #print(nts[1].time_on)
  print(',\n  '.join(['(%d /\ %d)' % (nt.note, nt.time_on) for nt in nts]))
