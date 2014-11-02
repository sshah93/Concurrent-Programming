import java.util.HashMap;
import java.util.Set;
import java.util.ArrayList;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Collections;
import java.util.Comparator;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class TokenCount {
	private static final HashMap<String, Integer> tokenFreq = new HashMap<String, Integer>();

	public static void main(String[] args) throws Exception {
		if (args.length != 2) {
			System.out
					.println("usage: java TokenCount number-of-pages XML-file");
			System.exit(0);
		}

		Integer numPages = Integer.parseInt(args[0]);

		// print number of available processors
		System.out.println("Running Version 2");
		int num_processors = Runtime.getRuntime().availableProcessors();
		System.out.println(num_processors
				+ " available processors");

		/* create shared queue */
		ArrayBlockingQueue<Page> sharedQueue = new ArrayBlockingQueue<Page>(100);

		/* create the Producer and Consumer threads */
		Thread producer = new Thread(new Producer(sharedQueue, args[1],
				numPages, num_processors - 1));
		ExecutorService pool = Executors.newCachedThreadPool();

		/* begin timed code ... */
		final long before = System.nanoTime();

		// call the producer thread to split the pages and put it in the shared
		// queue
		producer.start();
		for(int i = 0; i < num_processors - 1; i++)
		{
			pool.submit(new Consumer(sharedQueue));
		}
		
		// join the consumer and producer threads
		producer.join();
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
			System.out.println(list.get(i).getKey() + " appears "
					+ list.get(i).getValue() + " times");
		}
	}

	public static synchronized void countToken(String tok) {
		Integer currentCount = tokenFreq.get(tok);
		if (currentCount == null)
			tokenFreq.put(tok, 1);
		else
			tokenFreq.put(tok, currentCount + 1);
	}
}
