import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;
import java.util.TreeSet;

/**
 * Substitute to the very slow R post-processing source code.
 * This is a completely <i>ad hoc</i> implementation.
 * 
 * @author Vincent Labatut
 */
public class Main 
{
	/** Number formatter used to force two digits in episodes and season numbers */
	private final static NumberFormat FORMAT = NumberFormat.getInstance();
	static
	{	FORMAT.setMinimumIntegerDigits(2);
		FORMAT.setMaximumFractionDigits(0);
	}

	/**
	 * Main function takes two parameters to select the data folder and season.
	 * 
	 * @param args
	 * 		First argument is the data folder path, second one is an integer indicating
	 * 		the season number, or 0 to process all seasons at once.
	 * 
	 * @throws FileNotFoundException
	 * 		Problem while reading/writing one of the files.
	 * @throws UnsupportedEncodingException
	 * 		Encoding problem while writing the Graphml file.
	 */
	public static void main(String[] args) throws FileNotFoundException, UnsupportedEncodingException
	{	String dataFolder = args[0];
		final int season = Integer.parseInt(args[1]);
		if(season==0)
			log("Processing all seasons at once");
		else
			log("Processing only season "+season);
		
		// get graph files
		String[] graphFiles = null;
		File folder = new File(dataFolder);
		{	FilenameFilter filter;
			if(season==0)
				filter = new FilenameFilter()
				{	@Override
					public boolean accept(File arg0, String arg1)
					{	boolean result = arg1.toLowerCase().endsWith(".graphml");
						return result;
					}
				};
			else
				filter = new FilenameFilter()
				{	@Override
					public boolean accept(File arg0, String arg1)
					{	boolean result = arg1.toUpperCase().contains("S"+FORMAT.format(season)) && arg1.toLowerCase().endsWith(".graphml");
						return result;
					}
				};
			graphFiles = folder.list(filter);
			Arrays.sort(graphFiles);
		}
		
		// load community files
		List<List<TreeSet<Integer>>> allComs = new ArrayList<List<TreeSet<Integer>>>();
		{	FilenameFilter filter;
			if(season==0)
				filter = new FilenameFilter()
				{	@Override
					public boolean accept(File arg0, String arg1)
					{	boolean result = arg1.toLowerCase().endsWith("_cl.txt");
						return result;
					}
				};
			else
				filter = new FilenameFilter()
				{	@Override
					public boolean accept(File arg0, String arg1)
					{	boolean result = arg1.toUpperCase().contains("S"+FORMAT.format(season)) && arg1.toLowerCase().endsWith("_cl.txt");
						return result;
					}
				};
			String[] comFiles = folder.list(filter);
			Arrays.sort(comFiles);
			for(String comFile: comFiles)
			{	List<TreeSet<Integer>> tmp = new ArrayList<TreeSet<Integer>>();
				String path = dataFolder + File.separator + comFile;
				FileInputStream fileIn = new FileInputStream(path);
				InputStreamReader reader = new InputStreamReader(fileIn);
				Scanner scanner = new Scanner(reader);
				while(scanner.hasNextLine())
				{	String str = scanner.nextLine().trim();
					String[] strtab = str.split(" ");
					TreeSet<Integer> tmp2 = new TreeSet<Integer>();
					for(String nbr: strtab)
						tmp2.add(Integer.parseInt(nbr));
					tmp.add(tmp2);
				}
				allComs.add(tmp);
				scanner.close();
			}
		}
		
		// load the centrality file
		Float[] centr = null;
		Map<Float,List<Integer>> centr2id = new HashMap<Float,List<Integer>>();
		{	String centrFile;
			if(season==0)
				centrFile = dataFolder + File.separator + "all_centr.txt";
			else
				centrFile = dataFolder + File.separator + "season" + season + "_centr.txt";
			List<Float> centrTmp = new ArrayList<Float>();
			FileInputStream fileIn = new FileInputStream(centrFile);
			InputStreamReader reader = new InputStreamReader(fileIn);
			Scanner scanner = new Scanner(reader);
			int nodeId = 0;
			while(scanner.hasNextLine())
			{	float val = Float.parseFloat(scanner.nextLine());
				centrTmp.add(val);
				List<Integer> l = centr2id.get(val);
				if(l==null)
				{	l = new ArrayList<Integer>();
					centr2id.put(val,l);
				}
				l.add(nodeId);
				nodeId++;
			}
			scanner.close();
			centr = centrTmp.toArray(new Float[centrTmp.size()]);
		}
		
		// load the node names
		String[] charNames = null;
		{	String charFile = dataFolder + File.separator + "characters.txt";
			List<String> charTmp = new ArrayList<String>();
			FileInputStream fileIn = new FileInputStream(charFile);
			InputStreamReader reader = new InputStreamReader(fileIn);
			Scanner scanner = new Scanner(reader);
			while(scanner.hasNextLine())
			{	String line = scanner.nextLine().trim();
				line = line.replace("\"","");
				line = line.replace("\\","");
				charTmp.add(line);
			}
			scanner.close();
			charNames = charTmp.toArray(new String[charTmp.size()]);
		}
		
		// convert the timeline to a meta-graph format
		log("Convert to a meta-graph (season "+season+")");
		
		// load the file
		String[] str = null;
		{	String timelineFile;
			if(season==0)
				timelineFile = dataFolder + File.separator + "all.timeline";
			else
				timelineFile = dataFolder + File.separator + "season" + season + ".timeline";
			List<String> strTmp = new ArrayList<String>();
			FileInputStream fileIn = new FileInputStream(timelineFile);
			InputStreamReader reader = new InputStreamReader(fileIn);
			Scanner scanner = new Scanner(reader);
			while(scanner.hasNextLine())
				strTmp.add(scanner.nextLine());
			scanner.close();
			str = strTmp.toArray(new String[strTmp.size()]);
		}
		
		// build an equivalent matrix
		log("Build the matrix ("+str.length+"x"+graphFiles.length+")");
		int[][] m = new int[str.length][graphFiles.length];
		for(int i=0;i<str.length;i++)
		{	String tmp = str[i].split(":")[1];
			String[] tmp2 = tmp.split(",");
			for(String cpl: tmp2)
			{	String tmp3[] = cpl.split("=");
				int val1 = Integer.parseInt(tmp3[0])-1;
				int val2 = Integer.parseInt(tmp3[1]);
				m[i][val1] = val2;
			}
		}
		
		// define the corresponding meta-graph: one meta-node represents one community at a given time slice
		log("Define meta graph as a pajek file");
		StringBuffer xml = new StringBuffer();
		xml.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
				+"<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\"\n"
				+"\t\txmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n"
				+"\t\txsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns\n"
				+"\t\thttp://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">\n\n"
				+"\t<key id=\"v_name\" for=\"node\" attr.name=\"name\" attr.type=\"string\"/>\n"
				+"\t<key id=\"v_x\" for=\"node\" attr.name=\"x\" attr.type=\"double\"/>\n"
				+"\t<key id=\"v_y\" for=\"node\" attr.name=\"y\" attr.type=\"double\"/>\n"
				+"\t<key id=\"v_posx\" for=\"node\" attr.name=\"posx\" attr.type=\"double\"/>\n"
				+"\t<key id=\"v_posy\" for=\"node\" attr.name=\"posy\" attr.type=\"double\"/>\n"
				+"\t<key id=\"v_content\" for=\"node\" attr.name=\"content\" attr.type=\"string\"/>\n"
				+"\t<key id=\"v_contentsmpl\" for=\"node\" attr.name=\"contentsmpl\" attr.type=\"string\"/>\n"
				+"\t<key id=\"v_label\" for=\"node\" attr.name=\"label\" attr.type=\"string\"/>\n"
				+"\t<key id=\"v_weight\" for=\"node\" attr.name=\"weight\" attr.type=\"int\"/>\n"
				+"\t<key id=\"v_avgcentr\" for=\"node\" attr.name=\"avgcentr\" attr.type=\"float\"/>\n"
				+"\t<key id=\"v_maxcentr\" for=\"node\" attr.name=\"maxcentr\" attr.type=\"float\"/>\n"
				+"\t<key id=\"e_content\" for=\"edge\" attr.name=\"content\" attr.type=\"string\"/>\n"
				+"\t<key id=\"e_label\" for=\"edge\" attr.name=\"label\" attr.type=\"string\"/>\n"
				+"\t<key id=\"e_weight\" for=\"edge\" attr.name=\"weight\" attr.type=\"int\"/>\n"
				+"\t<graph id=\"G\" edgedefault=\"directed\">\n");
		TreeSet<String> names = new TreeSet<String>();
		Map<String,Integer> name2ids = new HashMap<String,Integer>();
		Map<Integer,String> id2names = new HashMap<Integer,String>();
		StringBuffer edgesXml = new StringBuffer();
		Map<Integer,Set<Integer>> edges = new HashMap<Integer, Set<Integer>>();
		int prevY = 0;
		for(int i=0;i<str.length;i++)
		{	log("  Process row "+i+"/"+str.length);
			Integer curY = null;
			String prevName = null;
			int prevId = -1;
			TreeSet<Integer> prevCom = null;
			for(int j=0;j<graphFiles.length;j++)
			{	if(m[i][j]==0)
				{	prevName = null;
					prevCom = null;
					prevId = -1;
				}
				else
				{	String name = (j+1)+"-"+m[i][j];
					TreeSet<Integer> comNodes = allComs.get(j).get(m[i][j]-1);
					int curId;
					if((names.contains(name)))
						curId = name2ids.get(name);
					else
					{	curId = names.size();
						names.add(name);
						name2ids.put(name,curId);
						id2names.put(curId,name);
						StringBuffer content = new StringBuffer();
						StringBuffer contentSmpl = new StringBuffer();
						float maxCentr = Float.NEGATIVE_INFINITY;
						float avgCentr = 0;
						int mainNode = -1;
						for(int comNode: comNodes)
						{	String comNodeName = charNames[comNode-1];
							float c = centr[comNode-1];
							content.append("("+comNodeName+") ");
							if(c>0.2)
								contentSmpl.append("("+comNodeName+") ");
							avgCentr = avgCentr + c;
							if(c>maxCentr)
							{	maxCentr = c;
								mainNode = comNode;
							}
						}
						avgCentr = avgCentr / comNodes.size();
						content.deleteCharAt(content.length()-1);
						if(contentSmpl.length()>0)
							contentSmpl.deleteCharAt(contentSmpl.length()-1);
						int xpos = (j+1);//*10;
						if(curY==null)
						{	curY = prevY + 1;
							prevY = curY;
						}
//						int ypos = (i+1);//*10;
						xml.append("\t\t<node id=\"n"+curId+"\">\n"
								+"\t\t\t<data key=\"v_name\">"+name+"</data>\n"
								+"\t\t\t<data key=\"v_x\">"+xpos+"</data>\n"
								+"\t\t\t<data key=\"v_y\">"+curY+"</data>\n"
								+"\t\t\t<data key=\"v_posx\">"+xpos+"</data>\n"
								+"\t\t\t<data key=\"v_posy\">"+curY+"</data>\n"
								+"\t\t\t<data key=\"v_content\">"+content.toString()+"</data>\n"
								+"\t\t\t<data key=\"v_contentsmpl\">"+contentSmpl.toString()+"</data>\n"
								+"\t\t\t<data key=\"v_label\">"+charNames[mainNode-1]+"</data>\n"
								+"\t\t\t<data key=\"v_avgcentr\">"+avgCentr+"</data>\n"
								+"\t\t\t<data key=\"v_maxcentr\">"+maxCentr+"</data>\n"
								+"\t\t\t<data key=\"v_weight\">"+comNodes.size()+"</data>\n"
								+"\t\t</node>\n");
					}
					if(prevName!=null)
					{	Set<Integer> neighs = edges.get(prevId);
						if(neighs==null || !neighs.contains(curId))
						{	if(neighs==null)
							{	neighs = new TreeSet<Integer>();
								edges.put(prevId, neighs);
							}
							neighs.add(curId);
							TreeSet<Integer> inter = new TreeSet<Integer>(prevCom);
							inter.retainAll(comNodes);
							StringBuffer content = new StringBuffer();
							float maxCentr = Float.NEGATIVE_INFINITY;
							int mainNode = -1;
							for(int comNode: inter)
							{	String comNodeName = charNames[comNode-1];
								content.append("("+comNodeName+") ");
								float c = centr[comNode-1];
								if(c>maxCentr)
								{	maxCentr = c;
									mainNode = comNode;
								}
							}
							content.deleteCharAt(content.length()-1);
							edgesXml.append("\t\t<edge source=\"n"+prevId+"\" target=\"n"+curId+"\">\n"
								+"\t\t\t<data key=\"e_content\">"+content.toString()+"</data>\n"
								+"\t\t\t<data key=\"e_label\">"+charNames[mainNode-1]+"</data>\n"
								+"\t\t\t<data key=\"e_weight\">"+inter.size()+"</data>\n"
								+"\t\t</edge>\n");
						}
					}
					prevName = name;
					prevCom = comNodes;
					prevId = curId;
				}
			}
		}
		
		// record meta-graph
		xml.append(edgesXml.toString()
				+"\t</graph>\n"
				+"</graphml>");
		File outFolder = new File(dataFolder + "_updt");
		if(!outFolder.exists())
			outFolder.mkdirs();
		String graphFile;
		if(season==0)
			graphFile = dataFolder + "_updt" + File.separator + "all.graphml";
		else
			graphFile = dataFolder + "_updt" + File.separator + "season" + season + ".graphml";
		log("Record graph as "+graphFile);
		FileOutputStream fileOut = new FileOutputStream(graphFile);
		OutputStreamWriter writer = new OutputStreamWriter(fileOut,"UTF-8");
		PrintWriter printWriter = new PrintWriter(writer);
		printWriter.print(xml.toString());
		printWriter.close();
		
		log("Done");
	}
	
	/**
	 * Time-stamped loging.
	 * 
	 * @param msg
	 * 		Message to log.
	 */
	private static void log(String msg)
	{	Calendar currentTime = Calendar.getInstance();
		int y = currentTime.get(Calendar.YEAR);
		int m = currentTime.get(Calendar.MONTH);
		int d = currentTime.get(Calendar.DAY_OF_MONTH);
		int h = currentTime.get(Calendar.HOUR_OF_DAY);
		int min = currentTime.get(Calendar.MINUTE);
		int s = currentTime.get(Calendar.SECOND);
		
		System.out.println("["+y+"/"+(m+1)+"/"+d+" "+h+":"+min+":"+s+"] "+msg);
	}
}
