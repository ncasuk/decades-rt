
package horaceplot.plot;

import java.awt.*;
import java.util.*;
import java.io.*;
import java.net.*;
/**
 * Reads in map data and draws track plot.
 * 
 * @author Dave tiddeman (dave.tiddeman@metoffice.gov.uk)
 * @version 1
 */

public class mapplot extends zoomplot  implements java.awt.event.ItemListener
{

        /**
         * Horace connection
         */
        // The following function is a placeholder for control initialization.
        // You should call this function from a constructor or initialization function.
        

    /**
     * Map data
     */
    Vector bitsofLand;
    Vector areas;
    Vector places;
    String mapdata;
    private static final long serialVersionUID = 8561575;
    float[] maprange=new float[4];
//    float[][] planeshape={{0,8},{1,5},{4,3},
//                          {1,3},{1,1},{2,0},
//                          {-2,0},{-1,1},{-1,3},
//                          {-4,3},{-1,5},{0,8}};
	float[][] planeshape={
		{       0,    1233},
		{      67,    1158},
		{      80,    1017},
		{      82,     789},
		{     157,     767},
		{     157,     866},
		{     207,     866},
		{     207,     750},
		{     268,     733},
		{     268,     831},
		{     318,     831},
		{     318,     716},
		{     571,     642},
		{     574,     574},
		{      81,     621},
		{      72,     365},
		{      30,     151},
		{     244,      61},
		{     245,       0},
		{    -245,       0},
		{    -244,      61},
		{     -30,     151},
		{     -72,     365},
		{     -81,     621},
		{    -574,     574},
		{    -571,     642},
		{    -318,     716},
		{    -318,     831},
		{    -268,     831},
		{    -268,     733},
		{    -207,     750},
		{    -207,     866},
		{    -157,     866},
		{    -157,     767},
		{     -82,     789},
		{     -80,    1017},
		{     -67,    1158},
		{       0,    1233}};
    Checkbox checkplane;
    int[][] oldplane=null;
    boolean dodrawplane=false;
    int randomwait=0;
    /**
     * Previous status of the map data
     */
    public byte prevq=0;

   /**
    * Creates a new track plot
    */
    
   public mapplot(){
    super(180,-180,90,-90);
   
                //{{INIT_CONTROLS
                setSize(0,0);
                //}}
        }
   
   public mapplot(String mapd){
    this();
    mapdata=mapd;
   }

     /**
     * Initialize track plot and start reading data.
     */

    public void init()
    {
        // This method is derived from class zoomplot
        // to do: code goes here
     System.out.println("init map");
       readmap(mapdata);
     setDatamaxmin(maprange[2]/100,maprange[0]/100,
                     maprange[3]/100,maprange[1]/100);
     setBackground(Color.white);
     prevq=q.mapstatus;
     setmaxmin(maprange[2]/100,maprange[0]/100,
                     maprange[3]/100,maprange[1]/100);
     plotapp pq=(plotapp)getParent();
     int dx=5000/pq.norm;
     int dy=(pq.top*10+300)/pq.norm;
     checkplane=new Checkbox("Draw plane",false);
//     alwaysredraw=false;
     pq.add(checkplane);
     checkplane.addItemListener(this);
     checkplane.setBounds(dx,dy,1200/pq.norm,150/pq.norm);
    }

    /**
     * Draws Track
     * 
     * @param g Graphics context
     * @param xx Longitudes
     * @param yy Latitudes
     * @param i parameter index
     */

    public void drawPoints(Graphics g, float xx[], float yy[], int i)
    {
        // This method is derived from class zoomplot
        // to do: code goes here
       if((prevq!=0)&&(q.mapstatus!=prevq)){
         prevq=q.mapstatus;
         randomwait=(int)(Math.random()*100);
       }
       if(randomwait>0){
       	randomwait--;
       	if(randomwait==0){  
         readmap(mapdata);
         drawBackground(g);
         setDatamaxmin(maprange[2]/100,maprange[0]/100,
                     maprange[3]/100,maprange[1]/100);
       }   
       }
       g.setColor(colors[colours[i-1]]);
       if(oldplane!=null){
              g.setXORMode(Color.white);
//              drawSome(g,oldplane[0],oldplane[1],-1,12);
			  drawSome(g,oldplane[0],oldplane[1],-1,38);
              g.setPaintMode();
              oldplane=null;
       }
       g.setPaintMode();
       drawSome(g,xx,yy,symbols[i-1]);
       if(dodrawplane){
              oldplane=drawplane(xx[xx.length-1],yy[yy.length-1],q.status[1]);
              g.setXORMode(Color.white);
//              drawSome(g,oldplane[0],oldplane[1],-1,12);
		      drawSome(g,oldplane[0],oldplane[1],-1,38);
              g.setPaintMode();
       }
              
    }

	/**
	 * Draws plane pointing in direction of heading
	 * 
	 * @param lon longitude
	 * @param lat latitude
	 * @param hdg heading
	 */                                                                                                                                                                                                                                               

    public int[][] drawplane(float lon,float lat,float hdg){
//        int[][] planec=new int[2][12];
		int[][] planec=new int[2][38];
        Dimension dpos=convert(lon,lat);
        float coshead=(float)(Math.cos((hdg-90)*Math.PI/180)*2);
        float sinhead=(float)(Math.sin((hdg-90)*Math.PI/180)*2);
//        for(int i=0;i<12;i++){
		for(int i=0;i<38;i++){
//          planec[0][i]=dpos.width+(int)(sinhead*planeshape[i][0]+coshead*planeshape[i][1]); 
//          planec[1][i]=dpos.height-(int)(coshead*planeshape[i][0]-sinhead*planeshape[i][1]); 
		  planec[0][i]=dpos.width+(int)((sinhead*planeshape[i][0]+coshead*planeshape[i][1])/50); 
		  planec[1][i]=dpos.height-(int)((coshead*planeshape[i][0]-sinhead*planeshape[i][1])/50); 
        }
        //drawSome(g,planec[0],planec[1],-1,12);
        return planec;
    }

	/**
	 * Draws map background
	 * 
	 * @param g Graphics context
	 */                                                                                                                                                                                                                                               

    public void drawBackground(Graphics g)
    {
        // This method is derived from class zoomplot
        // to do: code goes here
        oldplane=null;
        g.setPaintMode();
        g.setFont(new Font("SansSerif",Font.BOLD,fontSize));
		float[] lats;
		float[] lons;
		g.setColor(Color.green);
		int n=bitsofLand.size();
		for(int f=0;f<n;f++){
		   country c=(country)bitsofLand.elementAt(f);
		   lats=c.getLats();
		   lons=c.getLongs();
		   drawPolyline(g,lats,lons);
		}
        Color lightcyan=new Color(170,255,255);       
        float qly=(ymax+ymin)/2;
        float dly=xmax-xmin;
		int ddly=1;
        if(dly<3.0)ddly=10;
        if(dly<1.0)ddly=100;
        dly=dly*ddly;
		float qlx=(xmax+xmin)/2;
		float dlx=ymax-ymin;
		int ddlx=1;
		if(dlx<3.0)ddlx=10;
		if(dlx<1.0)ddlx=100;
		dlx=dlx*ddlx;
		java.text.DecimalFormat DF1=new java.text.DecimalFormat("##0");
		java.text.DecimalFormat DF2=new java.text.DecimalFormat("##0.0");
		java.text.DecimalFormat DF3=new java.text.DecimalFormat("##0.00");
		java.text.DecimalFormat DFx=DF1;
		java.text.DecimalFormat DFy=DF1;
		if(ddly==10)DFy=DF2;
		if(ddlx==10)DFx=DF2;
		if(ddly==100)DFy=DF3;
		if(ddlx==100)DFx=DF3;
		g.setColor(lightcyan); 
        for(int l=(int)(xmin-1)*ddly;l<(ddly*xmax);l+=1){
			if(dly<10){StringBuffer sb=new StringBuffer().append(DFy.format((float)(Math.abs(l))/ddly));                       
			drawString(g,(float)l/(float)ddly,qly,sb.toString());}
            drawLine(g,(float)l/(float)ddly,ymin,(float)l/(float)ddly,ymax);}
        for(int l=((int)ymin-1)*ddlx;l<(ddlx*ymax);l+=1){
			if(dlx<10){StringBuffer sb=new StringBuffer().append(DFx.format((float)(Math.abs(l))/ddlx));			
            drawString(g,qlx,(float)l/(float)ddlx,sb.toString());}
            drawLine(g,xmin,(float)l/(float)ddlx,xmax,(float)l/(float)ddlx);}
		g.setColor(Color.cyan);
		for(int l=((int)((xmin-10)/10))*10*ddly;l<(ddly*xmax);l+=10){
			StringBuffer sb=new StringBuffer().append(DFy.format((float)(Math.abs(l))/ddly));                       
			if(l>0){sb.append("E");}
			if(l<0){sb.append("W");}
			drawString(g,(float)l/(float)ddly,qly,sb.toString());
			drawLine(g,(float)l/(float)ddly,ymin,(float)l/(float)ddly,ymax);}
		for(int l=((int)((ymin-10)/10))*10*ddlx;l<(ddlx*ymax);l+=10){
			StringBuffer sb=new StringBuffer().append(DFx.format((float)(Math.abs(l))/ddlx));
			if(l>0){sb.append("N");}
			if(l<0){sb.append("S");}
			drawString(g,qlx,(float)l/(float)ddlx,sb.toString());
			drawLine(g,xmin,(float)l/(float)ddlx,xmax,(float)l/(float)ddlx);}
		g.setColor(Color.blue);	
		n=places.size();
		for(int f=0;f<n;f++){
          drawPoint(g,((place)places.elementAt(f)).Lon,((place)places.elementAt(f)).Lat,X);
		  drawString(g,((place)places.elementAt(f)).Lon,((place)places.elementAt(f)).Lat," "+((place)places.elementAt(f)).Name);
		}
//		drawString(g,(float)-0.62,(float)52.07,"* Cranfield");
//		drawString(g,(float)-3.4,(float)50.73,"* Exeter");
//		float[] blats={(float) 50.0,(float) 51.0,(float) 50.0,(float) 50.0};
//		float[] blons={(float) 0.0,(float) 1.0,(float) 2.0,(float) 0.0};
		g.setColor(Color.magenta);	
		n=areas.size();
		for(int f=0;f<n;f++){
		  drawPolyline(g,((area)areas.elementAt(f)).Lons,((area)areas.elementAt(f)).Lats);
		}
//		drawPolyline(g,blons,blats);
     }

   /**
    * Reads in Map dataset
    * 
    * @param mapdata String with name of map dataset
    */
    
   public void readmap (String mapdata){
        System.out.println("read map"); 
        bitsofLand=new Vector();  
        try{
            URL mapurl=new URL(mapdata);
            DataInputStream mapdat=new DataInputStream(mapurl.openStream());
            boolean reading=true;
            try{
					 /*These ones are ignored!*/
                maprange[0]=(float)mapdat.readShort();
                maprange[1]=(float)mapdat.readShort();
                maprange[2]=(float)mapdat.readShort();
                maprange[3]=(float)mapdat.readShort();
					 /*Just start with the whole globe*/
					 /*ranges are 100xlat, and 100xlong
						0=Lonmin, 1=latmin, 2=Lonmax, 3=latmax */
                maprange[0]=(float)-18000;
                maprange[1]=(float)-9000;
                maprange[2]=(float)18000;
                maprange[3]=(float)9000;
            }catch(IOException ioe){reading=false;}
            while(reading){
            try{
            short np=mapdat.readShort();
            short[] lats=new short[np];
            short[] lons=new short[np];
            for(short i=0;i<np;i++){
                lats[i]=mapdat.readShort();
                lons[i]=mapdat.readShort();
            }
            country c=new country(lats,lons);
            bitsofLand.addElement(c);
            }catch(IOException ioe){
                reading=false;
                mapdat.close();
            }
            }
        }catch(Exception e){}
        places=new Vector();
        areas=new Vector();
        try{ 
        	String[] s;
			int lastslash=mapdata.lastIndexOf("/")+1;
			String overlay=mapdata.substring(0,lastslash)+"overlay.txt";
        	URL overlayurl=new URL(overlay);
			BufferedReader reader=new BufferedReader(new InputStreamReader(overlayurl.openStream()));       
			boolean open=true;
			while(open){
					String Line=reader.readLine();
					if(Line!=null){
						while(Line.endsWith(",")){
							String Lplus=reader.readLine();
							if(Lplus!=null){
								Line=Line+Lplus;
							}
						}
						int count=horaceplot.stringutl.countseps(Line,",");
						System.out.println(Line);
						System.out.println(count);
						if(count>2){
							s=horaceplot.stringutl.stringsep(Line,",",count);
				        if(count==3){
				        	System.out.println("Place "+s[0]);
				        	place p=new place(s[0],s[1],s[2]);
				        	places.addElement(p);
				        	System.out.println(places.size());
				        }else{
							System.out.println("Area "+s[0]);
							area a=new area(s);
							areas.addElement(a);
				        }	
						}	
					}else{
						open=false;
					}
			}
		}catch(Exception e){
		  System.out.println(e);
		}
        
    
    }


        //{{DECLARE_CONTROLS
        //}}
	public void print(Graphics g){
	    if(checkplane!=null)checkplane.setVisible(false);
	    super.print(g);
	}
	public void paint(Graphics g){
	    if(checkplane!=null)checkplane.setVisible(true);
	    super.paint(g);
	}
	
	public void itemStateChanged(java.awt.event.ItemEvent event){
	  dodrawplane=checkplane.getState();
	  repaint();
	}
	class place{
		float Lat,Lon;
		String Name;
		
	    public place(String sname,String slat,String slon){
	    	Name=sname;
	    	Float F=new Float(slat);
	    	Lat=F.floatValue();
			F=new Float(slon);
			Lon=F.floatValue();
			System.out.println("*place "+sname);
	    }
	}
    class area{
    	float[] Lats,Lons;
    	
    	public area(String[] s){
    		Lats=new float[((s.length-1)/2)];
    		Lons=new float[((s.length-1)/2)];
    		for(int i=1;i<(s.length-1);i=i+2){
				Float F=new Float(s[i]);
    			Lats[i/2]=F.floatValue();
				F=new Float(s[i+1]);
				Lons[i/2]=F.floatValue();
    		}
    	}
    }
}
