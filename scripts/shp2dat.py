#!/usr/bin/env python
'''Takes a GSHHG .shp file and produces a map_data.dat file for the Horace
Java mapplot.java sub-app'''

infile="GSHHS_shp/i/GSHHS_i_L1.shp" #intermediate-res
outfile="map_data.dat"
'''.dat file is packed file of Short integers, consisting of 4 bounding-box
shorts giving the extents, then an arbitrary number of polygons given by a short indicating the number of points, n, then n lat/long pairs.
Lats and Longs are stored at 100x so integers can be used'''

import shapefile
import struct

sf = shapefile.Reader(infile)
shapes = sf.shapes()
out = open(outfile,'w')
out.write(struct.pack('>hhhh',-18000,-9000,18000,9000))
for shape in shapes:
   #max short is 32767, so chunk the shape points with 1pt overlap
   chunks=[shape.points[x:x+32761] for x in xrange(0, len(shape.points), 32760)]
   if len(chunks) > 1:
      print "Chunking a length " +str(len(shape.points))+" polyline"
   for chunk in chunks:
      outlist = [len(chunk)]
      for point in chunk:
         outlist.append(int(point[0]*100))
         outlist.append(int(point[1]*100))
         #detect collisions caused by rounding
         if(len(outlist) > 3 and (outlist[-1] == outlist[-3]) and (outlist[-2] == outlist[-4])):
            print 'Collision: ' + str([outlist[-2],outlist[-1]]) + ' and ' + str([outlist[-4],outlist[-3]])
            #remove duplicate
            outlist.pop()
            outlist.pop()
      #ensure shape is closed
      outlist.append(outlist[1])
      outlist.append(outlist[2])

      outlist[0] = len(outlist[1:])/2 #number of co-ordinate *pairs*
      fmt = '>' + str(len(outlist))+'h'
      out.write(struct.pack(fmt,*outlist))

