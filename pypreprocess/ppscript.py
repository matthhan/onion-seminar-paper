from imposm.parser import OSMParser

class CoordsFinder(object):
    fil = open("output.csv", "w+")
    def coords(self, coords):
        for osmid, lon, lat in coords:
            self.fil.write(str(osmid) + "," + str(lon) + "," + str(lat) + "\n")

counter = CoordsFinder()
p = OSMParser(concurrency=4, coords_callback=counter.coords)

p.parse('south-america-latest.osm.pbf')

