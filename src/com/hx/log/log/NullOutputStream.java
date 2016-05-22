/**
 * file name : NullOutputStream.java
 * created at : 9:54:39 PM Apr 15, 2016
 * created by 970655147
 */

package com.hx.log.log;

import java.io.IOException;
import java.io.OutputStream;

// 将给定的需要写出的数据丢弃
public class NullOutputStream extends OutputStream {

	// doNothing
	@Override
	public void write(int b) throws IOException {

	}

}
