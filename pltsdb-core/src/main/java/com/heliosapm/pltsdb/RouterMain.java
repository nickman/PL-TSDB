/**
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
 */
package com.heliosapm.pltsdb;




import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.lang.management.ManagementFactory;
import java.net.InetSocketAddress;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Properties;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicInteger;

import com.heliosapm.utils.buffer.BufferManager;
import com.heliosapm.utils.concurrency.ExtendedThreadManager;
import com.heliosapm.utils.config.ConfigurationHelper;
import com.heliosapm.utils.io.StdInCommandHandler;
import com.heliosapm.utils.jmx.JMXHelper;
import com.heliosapm.utils.url.URLHelper;

import io.netty.bootstrap.Bootstrap;
import io.netty.bootstrap.ServerBootstrap;
import io.netty.channel.AbstractChannel;
import io.netty.channel.Channel;
import io.netty.channel.ChannelFuture;
import io.netty.channel.ChannelOption;
import io.netty.channel.EventLoopGroup;
import io.netty.channel.ServerChannel;
import io.netty.channel.epoll.EpollDatagramChannel;
import io.netty.channel.epoll.EpollEventLoopGroup;
import io.netty.channel.epoll.EpollServerSocketChannel;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.nio.NioDatagramChannel;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import io.netty.handler.logging.LogLevel;
import io.netty.handler.logging.LoggingHandler;
import io.netty.util.concurrent.Future;
import io.netty.util.concurrent.GenericFutureListener;



/**
 * <p>Title: RouterMain</p>
 * <p>Description: The PL/TSDB Router Bootstrap</p> 
 * <p>Company: Helios Development Group LLC</p>
 * @author Whitehead (nwhitehead AT heliosdev DOT org)
 * <p><code>com.heliosapm.pltsdb.RouterMain</code></p>
 */
//@SpringBootApplication
//@ImportResource("classpath:router.xml")
public class RouterMain  {
	/** Static Class logger */
	protected static final Logger log = LogManager.getLogger(RouterMain.class);
	/** Indicates if we're on linux in which case, async will use epoll */
	public static final boolean IS_LINUX = System.getProperty("os.name").toLowerCase().contains("linux");
	/** The number of core available to this JVM */
	public static final int CORES = ManagementFactory.getOperatingSystemMXBean().getAvailableProcessors();
	/** The port to listen on */
	protected final int port;
	/** The nic interface to bind to */
	protected final String bindInterface;
	/** The socket address that the listener will be bound to */
	protected final InetSocketAddress bindSocket;	
	/** Indicates if we're using asynchronous net io */
	protected final boolean async;
	/** Indicates if epoll has been disabled even if we're on linux and using asynchronous net io */
	protected final boolean disableEpoll;
	
	/** The initialized RouterMain instance */
	protected static RouterMain routerMain = null;
	
	/** The netty TCP server bootstrap */
	protected final ServerBootstrap tcpServerBootstrap = new ServerBootstrap();
	/** The netty UDP bootstrap */
	protected final Bootstrap udpBootstrap = new Bootstrap();
	
	/** The configured number of worker threads */
	protected final int workerThreads;
	
	/** The TCP channel type this server will create */
	protected final Class<? extends ServerChannel> tcpChannelType;
	/** The UDP channel type this server will create */
	protected final Class<? extends AbstractChannel> udpChannelType;
	
	/** The netty boss event loop group */
	protected final EventLoopGroup bossGroup;
	/** The netty boss event loop group's executor and thread factory */
	protected final Executor bossExecutorThreadFactory;
	
	
	/** The netty worker event loop group */
	protected final EventLoopGroup workerGroup;
	/** The netty worker event loop group's executor and thread factory */
	protected final Executor workerExecutorThreadFactory;
	
	/** The netty TCP pipeline factory */
	protected final PipelineFactory tcpPipelineFactory;
	/** The netty UDP pipeline factory */
	protected final UDPPipelineFactory udpPipelineFactory;
	
	
	/** The TCP server channel created on socket bind */
	protected Channel tcpServerChannel = null;
	/** The UDP channel created  */
	protected Channel udpServerChannel = null;
	
	
	/** The TCP server's close future */
	protected ChannelFuture tcpCloseFuture = null;
	/** The UDP server's close future */
	protected ChannelFuture udpCloseFuture = null;
	
	
	// =============================================
	// Channel Configs
	// =============================================
	/** The size of the server socket's backlog queue */
	protected final int backlog;
	/** Indicates if reuse address should be enabled */
	protected final boolean reuseAddress;
	/** The server's connect timeout in ms */
	protected final int connectTimeout;
	
	
	// =============================================
	// Child Channel Configs
	// =============================================
	/** Indicates if tcp no delay should be enabled */
	protected final boolean tcpNoDelay;
	/** Indicates if tcp keep alive should be enabled */
	protected final boolean keepAlive;
	/** The write spin count */
	protected final int writeSpins;
	/** The size of a channel's receive buffer in bytes */
	protected final int recvBuffer;
	/** The size of a channel's send buffer in bytes */
	protected final int sendBuffer;
	
	/** The TCP server URI */
	public final URI tcpServerURI;
	/** The UDP server URI */
	public final URI udpServerURI;
	/** The boot configs */
	protected Properties bootConfig = null;

	static {
		System.setProperty("java.net.preferIPv4Stack", "true");
		ExtendedThreadManager.install();
		initPoolParam();
	}
	

    public static void main(String... args) {
		ExtendedThreadManager.install();
//		=============================================================================
		Properties config = null;
		if(args.length>0) {
			if(URLHelper.isValidURL(args[0])) {
				try { config = URLHelper.readProperties(URLHelper.toURL(args[0])); } catch(Exception ex) {}
			}
		}
		if(config==null) {
			config = URLHelper.readProperties(RouterMain.class.getClassLoader().getResource("defaultConfig.properties"));
		}		
		routerMain = new RouterMain(config);
		final Thread MAIN = Thread.currentThread();
		StdInCommandHandler.getInstance().registerCommand("shutdown", new Runnable(){
			public void run() {
				log.info("StdIn Handler Shutting Down RouterMain....");
				MAIN.interrupt();
				
			}
		}).runAsync(true).join();
    }
    
	private final Thread shutdownHook = new Thread() {
		public void run() {
			
			if(workerGroup!=null) {				
				log.info(">>>>> Shutting down OnRamp Listeners....");
				tcpServerChannel.close().syncUninterruptibly();
				udpServerChannel.close().syncUninterruptibly();
				workerGroup.shutdownGracefully().syncUninterruptibly();
				log.info("<<<<< OnRamp Listeners Shutdown");
			}
		}
	};

	/**
	 * Creates a new RouterMain
	 * @param appConfig  The application configuration
	 */
	public RouterMain(final Properties appConfig) {
		final String jmxmpUri = ConfigurationHelper.getSystemThenEnvProperty("jmx.jmxmp.uri", "jmxmp://0.0.0.0:1893", appConfig);
		JMXHelper.fireUpJMXMPServer(jmxmpUri);
//		MessageForwarder.initialize(appConfig);
		port = ConfigurationHelper.getIntSystemThenEnvProperty("onramp.network.port", 8091, appConfig);
		bindInterface = ConfigurationHelper.getSystemThenEnvProperty("onramp.network.bind", "0.0.0.0", appConfig);
		bindSocket = new InetSocketAddress(bindInterface, port);
		workerThreads = ConfigurationHelper.getIntSystemThenEnvProperty("onramp.network.worker_threads", CORES * 2, appConfig);
		connectTimeout = ConfigurationHelper.getIntSystemThenEnvProperty("onramp.network.sotimeout", 0, appConfig);
		backlog = ConfigurationHelper.getIntSystemThenEnvProperty("onramp.network.backlog", 3072, appConfig);
		writeSpins = ConfigurationHelper.getIntSystemThenEnvProperty("onramp.network.writespins", 16, appConfig);
		recvBuffer = ConfigurationHelper.getIntSystemThenEnvProperty("onramp.network.recbuffer", 43690, appConfig);
		sendBuffer = ConfigurationHelper.getIntSystemThenEnvProperty("onramp.network.sendbuffer", 8192, appConfig);
		disableEpoll =  ConfigurationHelper.getBooleanSystemThenEnvProperty("onramp.network.epoll.disable", false, appConfig);
		async = ConfigurationHelper.getBooleanSystemThenEnvProperty("onramp.network.async_io", true, appConfig);
		tcpNoDelay = ConfigurationHelper.getBooleanSystemThenEnvProperty("onramp.network.tcp_no_delay", true, appConfig);
		keepAlive = ConfigurationHelper.getBooleanSystemThenEnvProperty("onramp.network.keep_alive", true, appConfig);
		reuseAddress = ConfigurationHelper.getBooleanSystemThenEnvProperty("onramp.network.reuse_address", true, appConfig);		
		tcpPipelineFactory = new PipelineFactory(appConfig);
		udpPipelineFactory = new UDPPipelineFactory();
		tcpServerBootstrap.handler(new LoggingHandler(getClass(), LogLevel.INFO));
		tcpServerBootstrap.childHandler(tcpPipelineFactory);
		// Set the child options
		tcpServerBootstrap.childOption(ChannelOption.ALLOCATOR, BufferManager.getInstance().getAllocator());
		tcpServerBootstrap.childOption(ChannelOption.TCP_NODELAY, tcpNoDelay);
		tcpServerBootstrap.childOption(ChannelOption.SO_KEEPALIVE, keepAlive);
		tcpServerBootstrap.childOption(ChannelOption.SO_RCVBUF, recvBuffer);
		tcpServerBootstrap.childOption(ChannelOption.SO_SNDBUF, sendBuffer);
		tcpServerBootstrap.childOption(ChannelOption.WRITE_SPIN_COUNT, writeSpins);
		// Set the server options
		tcpServerBootstrap.option(ChannelOption.SO_BACKLOG, backlog);
		tcpServerBootstrap.option(ChannelOption.SO_REUSEADDR, reuseAddress);
		tcpServerBootstrap.option(ChannelOption.SO_RCVBUF, recvBuffer);
		tcpServerBootstrap.option(ChannelOption.SO_TIMEOUT, connectTimeout);
		
		
		
		
		final StringBuilder tcpUri = new StringBuilder("tcp");
		final StringBuilder udpUri = new StringBuilder("udp");
		if(IS_LINUX && !disableEpoll) {
			bossExecutorThreadFactory = new ExecutorThreadFactory("EpollServerBoss", true);
			bossGroup = new EpollEventLoopGroup(1, (ThreadFactory)bossExecutorThreadFactory);
			workerExecutorThreadFactory = new ExecutorThreadFactory("EpollServerWorker", true);
			workerGroup = new EpollEventLoopGroup(workerThreads, (ThreadFactory)workerExecutorThreadFactory);
			tcpChannelType = EpollServerSocketChannel.class;
			udpChannelType = EpollDatagramChannel.class;
			tcpUri.append("epoll");
			udpUri.append("epoll");
		} else {
			bossExecutorThreadFactory = new ExecutorThreadFactory("NioServerBoss", true);
			bossGroup = new NioEventLoopGroup(1, bossExecutorThreadFactory);
			workerExecutorThreadFactory = new ExecutorThreadFactory("NioServerWorker", true);
			workerGroup = new NioEventLoopGroup(workerThreads, workerExecutorThreadFactory);
			tcpChannelType = NioServerSocketChannel.class;
			udpChannelType = NioDatagramChannel.class;
			tcpUri.append("nio");
			udpUri.append("nio");
		}
		
		tcpUri.append("://").append(bindInterface).append(":").append(port);
		udpUri.append("://").append(bindInterface).append(":").append(port);
		URI u = null;
		try {
			u = new URI(tcpUri.toString());
		} catch (URISyntaxException e) {
			log.warn("Failed TCP server URI const: [{}]. Programmer Error", tcpUri, e);
		}
		tcpServerURI = u;
		try {
			u = new URI(udpUri.toString());
		} catch (URISyntaxException e) {
			log.warn("Failed UDP server URI const: [{}]. Programmer Error", udpUri, e);
		}
		udpServerURI = u;
		
		log.info(">>>>> Starting OnRamp TCP Listener on [{}]...", tcpServerURI);
		log.info(">>>>> Starting OnRamp UDP Listener on [{}]...", udpServerURI);
		final ChannelFuture cf = tcpServerBootstrap
			.channel(tcpChannelType)
			.group(bossGroup, workerGroup)
			.bind(bindSocket)
			.awaitUninterruptibly()
			.addListener(new GenericFutureListener<Future<? super Void>>() {
				public void operationComplete(final Future<? super Void> f) throws Exception {
					log.info("<<<<< OnRamp TCP Listener on [{}] Started", tcpServerURI);					
				};
			}).awaitUninterruptibly();
		final ChannelFuture ucf = udpBootstrap
				.channel(udpChannelType)
				.group(workerGroup)
				.option(ChannelOption.SO_BROADCAST, true)
				.handler(new UDPPipelineFactory())
				.bind(bindSocket)
				.awaitUninterruptibly()
				.addListener(new GenericFutureListener<Future<? super Void>>() {
					public void operationComplete(final Future<? super Void> f) throws Exception {
						log.info("<<<<< OnRamp UDP Listener on [{}] Started", udpServerURI);					
					};
				}).awaitUninterruptibly();
		
		tcpServerChannel = cf.channel();
		udpServerChannel = ucf.channel();
		tcpCloseFuture = tcpServerChannel.closeFuture();
		udpCloseFuture = udpServerChannel.closeFuture();
		Runtime.getRuntime().addShutdownHook(shutdownHook);
	}
	
	private static void initPoolParam() {
		try {
			Class.forName("io.netty.buffer.PooledByteBufAllocator", true, RouterMain.class.getClassLoader());
		} catch (Exception ex) {
			throw new RuntimeException("Failed to initialize pool params", ex);
		}
	}
	

    
	/**
	 * <p>Title: ExecutorThreadFactory</p>
	 * <p>Description: Combines an executor and thread factory</p> 
	 * <p>Company: Helios Development Group LLC</p>
	 * @author Whitehead (nwhitehead AT heliosdev DOT org)
	 * <p><code>com.heliosapm.pltsdb.RouterMain.ExecutorThreadFactory</code></p>
	 */
	public static class ExecutorThreadFactory implements Executor, ThreadFactory {
		final Executor executor;
		final ThreadFactory threadFactory;
		final String name;
		final AtomicInteger serial = new AtomicInteger();
		
		ExecutorThreadFactory(final String name, final boolean daemon) {
			this.name = name;
			threadFactory = new ThreadFactory() {
				@Override
				public Thread newThread(final Runnable r) {
					final Thread t = new Thread(r, name + "Thread#" + serial.incrementAndGet());
					t.setDaemon(daemon);
					return t;
				}
			};
			executor = Executors.newCachedThreadPool(threadFactory);
		}

		/**
		 * Executes the passed runnable in the executor
		 * @param command The runnable to execute
		 * @see java.util.concurrent.Executor#execute(java.lang.Runnable)
		 */
		@Override
		public void execute(final Runnable command) {
			executor.execute(command);
		}
		
		/**
		 * Creates a new thread
		 * {@inheritDoc}
		 * @see java.util.concurrent.ThreadFactory#newThread(java.lang.Runnable)
		 */
		@Override
		public Thread newThread(final Runnable r) {
			return threadFactory.newThread(r);
		}
	}
    


}
