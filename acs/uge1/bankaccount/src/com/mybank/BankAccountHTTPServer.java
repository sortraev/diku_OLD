package com.mybank;

import java.util.Random;

/**
 * {@link BankAccountHTTPServer} starts the bookstore HTTP server that the
 * clients will communicate with.
 */
public class BankAccountHTTPServer {
	
	/** The Constant DEFAULT_PORT */
	protected static final int DEFAULT_PORT = 8081;

	/** The Constant PROPERTY_KEY_SERVER_PORT. */
	public static final String PROPERTY_KEY_SERVER_PORT = "port";

	/**
	 * Prevents instantiation of a new {@link BankAccountHTTPServer}.
	 */
	private BankAccountHTTPServer() {
		// Prevent instantiation.
	}

	/**
	 * The main method.
	 *
	 * @param args
	 *            the arguments
	 */
	public static void main(String[] args) {
	  Random rand = new Random();

	  int bound = 100000;

	  for (int i = 0; i < 100; i++)
	  System.out.println(rand.nextInt(bound));
//		init_server(DEFAULT_PORT, PROPERTY_KEY_SERVER_PORT);
	}

	static void init_server(int defaultPort, String propertyKeyServerPort) {
		Account newba = new BankAccount(123456789);
		int listenOnPort = defaultPort;
		BankAccountHTTPMessageHandler handler = new BankAccountHTTPMessageHandler(newba);
		String serverPortString = System.getProperty(propertyKeyServerPort);

		if (serverPortString != null) {
			try {
				listenOnPort = Integer.parseInt(serverPortString);
			} catch (NumberFormatException ex) {
				System.err.println(ex);
			}
		}

		if (BankAccountHTTPServerUtility.createServer(listenOnPort, handler)) {
			// Do nothing.
		}
	}
}
