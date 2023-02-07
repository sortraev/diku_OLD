module acertainbookstore {
	exports com.acertainbookstore.client;
	exports com.acertainbookstore.business;
	exports com.acertainbookstore.interfaces;
	exports com.acertainbookstore.client.tests;
	exports com.acertainbookstore.server;
	exports com.acertainbookstore.utils;

	requires jetty.client;
	requires jetty.http;
	requires jetty.server;
	requires jetty.util;
	requires junit;
	requires kryo;
	requires objenesis;
	requires servlet.api;
	requires xstream;
}