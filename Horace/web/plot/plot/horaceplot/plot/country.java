
package horaceplot.plot;

/**
 * Object for Map data.
 * 
 * @author Dave tiddeman (dave.tiddeman@metoffice.gov.uk)
 * @version 1
 */
public class country
{

    /** Name of line */
    public String Name;
    /** Latitudes */
    public short[] lats;   
	/** Longitudes */
    public short[] longs;
    /**
     * Line on map
     * @param  latlon   Latitiude and Longitude in 2D array
     */
    public country(short[][] latlon){
        lats=latlon[0];
        longs=latlon[1];
    }

    /**
     * Line on map
     * 
     * @param lat Latitudes
     * @param lon Longitudes
     */
    public country(short[] lat,short[] lon){
        lats=lat;
        longs=lon;
    }

/**
 * Get latitudes from line
 * 
 * @return Latitudes
 */

   public float[] getLats(){
      int n=lats.length;
      float[] ans=new float[n];
      for(int i=0;i<n;i++){
         ans[i]=(float)lats[i]/100;
      }
      return ans;
   }

   /**
    * Get longitudes from line
    * 
    * @return Longitudes
    */
   public float[] getLongs(){
      int n=longs.length;
      float[] ans=new float[n];
      for(int i=0;i<n;i++){
         ans[i]=(float)longs[i]/100;
      }
      return ans;
   }
}
