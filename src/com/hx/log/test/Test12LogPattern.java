/**
 * file name : Test12LogPattern.java
 * created at : 10:33:36 2016-11-05
 * created by 970655147
 */

package com.hx.log.test;

import static com.hx.log.util.Log.info;

import org.junit.Test;

import com.hx.log.util.Constants;
import com.hx.log.util.LogPattern.LogPatternChain;
import com.hx.log.util.LogPatternUtils;
import com.hx.log.util.Tools;

import net.sf.json.JSONObject;

public class Test12LogPattern {
	
	@Test
	public void test01ForLineInfo() {
		String logPatternStr = "${stackTrace} // ${lineInfo} // ${handler(map('documentPrefix--' + $this))} ";
		LogPatternChain chain = LogPatternUtils.initLogPattern(logPatternStr);
		info(LogPatternUtils.formatLogInfo(chain, new JSONObject().element("lineInfo", "df")
				.element(Constants.HANDLER, "testHandler") 
				) );
		
		info(LogPatternUtils.formatLogInfo("{} this {}", 1, 2) );
		info(LogPatternUtils.formatLogInfo("{} this {} // {} || {}", 1, 2) );
		info(LogPatternUtils.formatLogInfo("{} this {} // {} || {}", 1, 2, "sdf", "++") );
		
		info(LogPatternUtils.formatLogInfo("$ this # // # || $", Tools.asSet("#", "$"), 1, 2, "sdf", "++") );
		
		info(LogPatternUtils.formatLogInfoWithIdx("this is first param {0}, and this is second param {1} {exp} .", "param01", "param02") );
		
	}

}
