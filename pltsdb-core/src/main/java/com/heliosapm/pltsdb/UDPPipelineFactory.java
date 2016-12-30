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

import java.nio.charset.Charset;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import io.netty.channel.AbstractChannel;
import io.netty.channel.ChannelInitializer;
import io.netty.channel.ChannelPipeline;
import io.netty.handler.codec.LineBasedFrameDecoder;

/**
 * <p>Title: UDPPipelineFactory</p>
 * <p>Description: The pipeline factory for the UDP stack</p> 
 * @author Whitehead (nwhitehead AT heliosdev DOT org)
 * <p><code>com.heliosapm.pltsdb.UDPPipelineFactory</code></p>
 */

public class UDPPipelineFactory extends ChannelInitializer<AbstractChannel> { 
	/** The UTF8 character set */
	public static final Charset UTF8 = Charset.forName("UTF8");
	
	/** The instance logger */
	protected final Logger log = LogManager.getLogger(getClass());

	protected final DatagramToBytesDecoder bytesDecoder = new DatagramToBytesDecoder();
	
	
	
	/**
	 * Creates a new UDPPipelineFactory
	 */
	public UDPPipelineFactory() {
	}


	/**
	 * {@inheritDoc}
	 * @see io.netty.channel.ChannelInitializer#initChannel(io.netty.channel.Channel)
	 */
	@Override
	protected void initChannel(final AbstractChannel ch) throws Exception {
		ChannelPipeline p = ch.pipeline();
		p.addLast("bytesDecoder", bytesDecoder);
		p.addLast("framer", new LineBasedFrameDecoder(1024, true, true));
		p.addLast("linehandler", new StringMetricHandler());
	}

//	/**
//	 * {@inheritDoc}
//	 * @see io.netty.channel.SimpleChannelInboundHandler#channelRead0(io.netty.channel.ChannelHandlerContext, java.lang.Object)
//	 */
//	@Override
//	protected void channelRead0(ChannelHandlerContext ctx, DatagramPacket msg) throws Exception {
//		// TODO Auto-generated method stub
//		
//	}

}



