from bs4 import BeautifulSoup
with open('squares.svg','r') as squares:
  soup = BeautifulSoup(squares.read(), 'html.parser')
  rects = soup.find_all('rect')
  for i,x in enumerate(rects):
    print('  ,' if i != 0 else "  ", 'T2 (Rect %s %s %s %s) (RGB 100 100 100) T2_0' %  (x.attrs['x'], x.attrs['y'], x.attrs['width'], x.attrs['height']))