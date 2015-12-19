from imposm.parser import OSMParser

class HighwayCounter(object):
        highways = 0

        def way(self,ways):
            for osmid, tags, refs in ways:
                if 'highway' in tags:
                        self.highways += 1

counter = HighwayCounter()
p = OSMParser(concurrency=4,ways_callback=counter.ways)
p.parse('central-america-latest.osm.pbf')

print(counter.highways)

