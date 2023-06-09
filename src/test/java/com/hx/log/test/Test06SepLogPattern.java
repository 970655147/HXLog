/**
 * file name : Test06SepLogPattern.java
 * created at : 10:03:22 PM Apr 21, 2016
 * created by 970655147
 */

package com.hx.log.test;

import static com.hx.log.util.Log.log;

import java.io.IOException;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import com.hx.log.log.LogPatternConstants;
import com.hx.log.log.log_pattern.LogPatternChain;
import com.hx.log.util.Log;
import com.hx.log.log.LogPatternUtils;
import com.hx.log.log.SimpleLogger;
import com.hx.log.util.Tools;
import com.hx.common.str.WordsSeprator;

import com.hx.json.JSONObject;

public class Test06SepLogPattern {

	// ���Էָ�logPattern
	public static void main(String[] args) throws IOException {
		
		String str = "${PREFIX } ${time } ${mode } ${msg } ~~~";
		Set<String> sepToPos = new HashSet<>();
		sepToPos.add("${");
		sepToPos.add("}");
		Tools.setDefaultCharSet(Tools.GBK);
		
//		Log.setOutToLogFile(true, Tools.getTmpPath("log1", Tools.LOG) );
		WordsSeprator sep = new WordsSeprator(str, sepToPos, null, true);
//		WordsSeprator sep = new WordsSeprator(str, sepToPos, null, true, true);
		while(sep.hasNext() ) {
			Log.log(sep.next() );
		}
		
//		Log.setLogFile(Tools.getTmpPath("log2", Tools.LOG) );
		Log.log("abc");
		
//		Log.flush();
//		Tools.awaitShutdown();
		
		new SimpleLogger().log();
		
		
		LogPatternChain logPattern = LogPatternUtils.initLogPattern(str);
		log(LogPatternUtils.formatLogInfo(logPattern, new JSONObject().element("PREFIX", "sdf").element("time", new Date().toString()).element("mode", "[LOG]").element("msg", "got sdf")) );
		
		
		String optionalStr = "${PREFIX } ${time } ${mode } ${msg } ~~~ $[${this} ---$[${opt} ||| ]--- ]";
		log(LogPatternUtils.formatLogInfo(LogPatternUtils.initLogPattern(optionalStr), 
				new JSONObject().element("PREFIX", "sdf").element("time", new Date().toString()).element("mode", "[LOG]")
				.element("msg", "got sdf").element("this", "$this$").element("opt", "#o#p#t#")) );
		log(LogPatternUtils.formatLogInfo(LogPatternUtils.initLogPattern(optionalStr), 
				new JSONObject().element("PREFIX", "sdf").element("time", new Date().toString()).element("mode", "[LOG]")
				.element("msg", "got sdf").element("this", "$this$")) );
		log(LogPatternUtils.formatLogInfo(LogPatternUtils.initLogPattern(optionalStr), 
				new JSONObject().element("PREFIX", "sdf").element("time", new Date().toString()).element("mode", "[LOG]")
				.element("msg", "got sdf")) );
		log(LogPatternUtils.formatLogInfo(LogPatternUtils.initLogPattern(optionalStr), 
				new JSONObject().element("PREFIX", "sdf").element("time", new Date().toString()).element("mode", "[LOG]")
				.element("msg", "got sdf").element("opt", "#o#p#t#")) );
		
		String str01 = "qw{}{}sdf";
		sep = new WordsSeprator(str01, Tools.asSet(LogPatternConstants.VAR_PLACE), null, true, true);
		while(sep.hasNext() ) {
			Log.err(sep.next() );
		}
		
	}
	
}
