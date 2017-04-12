/**
 * file name : IdxIterator.java
 * created at : 下午9:47:52 2016年8月11日
 * created by 970655147
 */

package com.hx.log.interf;

import com.hx.log.bit.BitMap;
import com.hx.log.util.Tools;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;

// 一个获取索引的迭代器
public interface IdxIterator {

	/**
	 * 是否还有下一个索引
	 *
	 * @return  true if next invoke of next() return idx, or else
	 * @author Jerry.X.He
	 * @date 4/12/2017 10:17 PM
	 * @since 1.0
	 */
	boolean hasNext();

	/**
	 * 获取下一个索引
	 *
	 * @return  next index
	 * @author Jerry.X.He
	 * @date 4/12/2017 10:17 PM
	 * @since 1.0
	 */
	int next();
	
}
