import java.util.Set;
import java.util.ArrayList;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Collections;
import java.util.Comparator;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class TokenCount {
	private static final ConcurrentHashMap<String, Integer> tokenFreq = new ConcurrentHashMap<String, Integer>();

	public static void main(String[] args) throws Exception {
		if (args.length != 2) {
			System.out
					.println("usage: java TokenCount number-of-pages XML-file");
			System.exit(0);
		}

		// number of pages to parse
		Integer numPages = Integer.parseInt(args[0]);

		// figure out the number of consumer threads to make
		int num_processors = Runtime.getRuntime().availableProcessors();
		
		// print number of available processors
		System.out.println("Running Version 4");
		System.out.println(num_processors + " available processors");

		/* create shared queue */
		ArrayBlockingQueue<Page> sharedQueue = new ArrayBlockingQueue<Page>(100);

		/* create the Producer and Consumer threads */
		Thread producer = new Thread(new Producer(sharedQueue, args[1],
				numPages, num_processors-1));
		//Thread consumer = new Thread(new Consumer(sharedQueue));
		
		// thread pool of consumer threads
		ExecutorService pool = Executors.newFixedThreadPool(num_processors-1);
		
		/* begin timed code ... */
		final long before = System.nanoTime();

		// call the producer thread to split the pages and put it in the shared
		// queue
		producer.start();
		
		// initialize the thread pool
		for(int i = 0; i < num_processors - 1; i++)
		{
			pool.submit(new Consumer(sharedQueue, tokenFreq));
		}
		
		// join the thread
		producer.join();
		
		// shutdown and terminate the thread pool
		pool.shutdown();
		pool.awaitTermination(1, TimeUnit.SECONDS);
		
		// on each page, find all tokens then increase the count for each token
		final long after = System.nanoTime();
		/* ... end timed code */

		System.out.println("Time to process " + numPages + " pages = "
				+ (after - before) / 1000000 + " milliseconds");

		// sort tokenFreq by value & print top 30 most common tokens
		Set<Entry<String, Integer>> entries = tokenFreq.entrySet();
		ArrayList<Entry<String, Integer>> list = new ArrayList<Entry<String, Integer>>(
				entries);
		Collections.sort(list, new Comparator<Map.Entry<String, Integer>>() {
			public int compare(Map.Entry<String, Integer> obj1,
					Map.Entry<String, Integer> obj2) {
				return (obj2.getValue()).compareTo(obj1.getValue());
			}
		});

		for (int i = 0; i < 30; i++) {
			System.out.println(list.get(i).getKey() + " appears " + list.get(i).getValue() + " times");
		}
	}

	@SuppressWarnings("unused")
	private static void countToken(String tok) {
		Integer currentCount = tokenFreq.get(tok);
		if (currentCount == null)
			tokenFreq.put(tok, 1);
		else
			tokenFreq.put(tok, currentCount + 1);
	}
}