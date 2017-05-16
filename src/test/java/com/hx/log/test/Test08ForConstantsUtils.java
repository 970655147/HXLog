/**
 * file name : Test08ForConstantsUtils.java
 * created at : 下午6:15:00 2016年6月25日
 * created by 970655147
 */

package com.hx.log.test;

import com.hx.log.util.Constants;
import com.hx.log.util.ConstantsUtils;
import com.hx.log.util.Log;
import com.hx.log.util.Tools;

public class Test08ForConstantsUtils {

	// 测试ConstantsUtils
	public static void main(String[] args) throws Exception {
		
		Log.log.setLogPattern(Constants.JUST_PRINT_MSG_LOG_PATTERN);
		
//		String codes = ConstantsUtils.generateCodesWithStaticFields(Tools.getTmpPath("HXMongoConfig", Tools.CONF) );
//		Log.log(codes);
		
//		String codes = ConstantsUtils.generateCodesWithOpt(Tools.getTmpPath("HXMongoConfig", Tools.CONF) );
//		Log.log(codes);
		
		
		String codes = ConstantsUtils.generateCodesWithOpt(Tools.getTmpPath("HXMongoConfig", Tools.CONF) );
		Log.log(codes);
		
		
		
	}

}
