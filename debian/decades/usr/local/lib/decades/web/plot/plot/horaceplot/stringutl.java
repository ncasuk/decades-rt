/*
 * Created on Nov 1, 2005
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package horaceplot;

/**
 * String utilities for the horace plots 
 *
 * @author dave.tiddeman
 *
 */
public class stringutl {
	/**
	 * Count items in a list
	 * @param s  The list
	 * @param sep The seperator ( usually a comma or a space )
	 * @return The number of items
	 */
	public static int countseps(String s,String sep){
	  int n=0;
	  if(s!=null){
	   if(s.length()>0){
		int i=s.indexOf(sep)+1;
		n=1;
		while(i!=0){
		  i=s.indexOf(sep,i)+1;
		  n++;
		}
	   }
	  }
	  return n;
	}
    /**
     * Divide a list of integers
     * @param s The string
     * @param sep The seperator
     * @param n The maximum number of items ( if 0 all items )
     * @return An array of integers
     */
	public static int[] stringsepi(String s,String sep,int n){
		String[] ss;
		int[] ans;
		ss=stringsep(s,sep,n);
		if(n==0)n=ss.length;
		ans=new int[n];
//		  for(int i=0;i<n;i++)ans[i]=0;
		for(int i=0;(i<ss.length)&&(i<n);i++){
			ans[i]=Integer.parseInt(ss[i]);
		}
		return ans;
	}
    /**
     * Divides a string up into seperate items
     * @param s The string
     * @param sep The seperator
     * @param n The maximum number of items ( if 0 all items )
     * @return An array of strings
     */
	public static String[] stringsep(String s,String sep,int n){
	  String[] ans;
	  if(n==0)n=countseps(s,sep);
	  ans=new String[n];
	  int i1=0;
	  int i=s.indexOf(sep)+1;
	  for (int ii=0;ii<n;ii++){
		if(i==0){
		  ans[ii]=s.substring(i1);
		}else{
		  ans[ii]=s.substring(i1,i-1);
		}
		i1=i;
		i=s.indexOf(sep,i)+1;
	  }
	  return ans;
	}
    /**
     * Converts time string to seconds past midnight
     * @param s The string "HH:MM:SS"
     * @return Seconds past midnight
     */

	public static int secs(String s){
        
		int ans=-1;
		if(s==null)s="now";
		if(s.equals(""))s="now";
		if(s.equals("start")){ans=-2;}else{
		  if(!s.equals("now")){
			int[] ts=stringsepi(s,":",0);
			ans=ts[0]*3600;
			if(ts.length>1)ans+=ts[1]*60;
			if(ts.length>2)ans+=ts[2];
		  }
		}
		return ans;
	}
	/**
	 * Adds leading zeros to numbers
	 * @param a number
	 * @param n number of digits you want in the number
	 * @return the number in a string
	 */
	private static String lead0(int a,int n){
	  StringBuffer ans=new StringBuffer();
	  ans.append(a);
	  while(ans.length()<n)ans.insert(0,"0");
	  return ans.toString();
	}
  /**
   * Converts a floating point number to a time sting
   * @param val Seconds past midnight
   * @return "HH:MM:SS"
   */
	public static String gmt(float val){
	  String ans;
	  int h=(int)(Math.floor(val/3600));
	  float l=(val-(float)h*3600);
	  int m=(int)Math.floor(l/60);
	  l=(l-(float)m*60);
	  int s=(int)Math.floor(l);
	  ans=lead0(h,2)+":"+lead0(m,2)+":"+lead0(s,2);
	  return ans;
	}

}
