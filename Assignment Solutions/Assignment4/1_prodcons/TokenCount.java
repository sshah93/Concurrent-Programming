import java.util.concurrent.ArrayBlockingQueue;
import java.util.HashMap;
import java.util.Set;
import java.util.ArrayList;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Collections;
import java.util.Comparator;


public class TokenCount {

    public static void main(String[] args) throws Exception {
	if (args.length != 2) {
	    System.out.println("usage: java TokenCount number-of-pages XML-file");
	    System.exit(0);
	}
	Integer numPages = Integer.parseInt(args[0]);

    // print number of available processors
	System.out.println(Runtime.getRuntime().availableProcessors() + " available processors");

	ArrayBlockingQueue<Page> sharedQueue = new ArrayBlockingQueue<Page>(100);
	HashMap<String, Integer> tokenFreq = new HashMap<String, Integer>();

	Thread counter = new Thread(new Consumer(sharedQueue, tokenFreq));
	Thread parser = new Thread(new Producer(sharedQueue, numPages, args[1]));

	final long before = System.nanoTime();
	counter.start();
	parser.start();
	parser.join();
	sharedQueue.put(new PoisonPill());
	counter.join();
	final long after = System.nanoTime();
	System.out.println("Time to process " + numPages + " pages = " + (after - before)/1000000 + " milliseconds");

    // sort tokenFreq by value & print top 30 most common tokens
	Set<Entry<String, Integer>> entries = tokenFreq.entrySet();
        ArrayList<Entry<String, Integer>> list = new ArrayList<Entry<String, Integer>>(entries);
        Collections.sort(list, new Comparator<Map.Entry<String, Integer>>()
			  {
			      public int compare(Map.Entry<String, Integer> obj1, Map.Entry<String, Integer> obj2)
			      {
				  return (obj2.getValue()).compareTo(obj1.getValue());
			      }
			  } );
        for(int i=0; i<30; i++)
            System.out.println(list.get(i).getKey() + " appears " + list.get(i).getValue() + " times");
    }

}
