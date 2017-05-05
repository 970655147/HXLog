/**
 * file name : BufferUtils.java
 * created at : 20:06:51 2016-12-30
 * created by 970655147
 */

package com.hx.log.io;

import com.hx.log.io.interf.BuffSizeEstimator;
import com.hx.log.io.interf.BufferHandler;
import com.hx.log.util.Log;
import com.hx.log.util.Tools;

import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import static com.hx.log.util.Tools.assert0;

public final class BufferManager {

    // ------------ 缓冲相关 ------- 2016.03.16 -------------

    /**
     * 默认的刷出buffer阈值
     */
    public static int DEFAULT_BUFF_THRESHOLD = 128 << 10;
    /**
     * 默认的buffSizeEstimator
     */
    public static BuffSizeEstimator DEFAULT_BUFFSIZE_ESTIMATOR = new BuffSizeEstimator() {
        public int getBuffSize(int threshold) {
            return threshold + (threshold >> 3);
        }
    };
    /**
     * 默认的buffHandler
     */
    public static BufferHandler DEFAULT_BUFF_HANDLER = new BufferHandler() {
        @Override
        public void beforeHandle(BuffInfo buffInfo) throws Exception {

        }

        @Override
        public void handleBuffer(BuffInfo buffInfo) throws Exception {
            flushBuffer(buffInfo.getSb(), buffInfo.getOutputPath(), buffInfo.getCharset());
        }

        @Override
        public void afterHandle(BuffInfo buffInfo) throws Exception {

        }
    };

    /**
     * buffName -> buffInfo
     */
    private Map<String, BuffInfo> bufferToBuffInfo = new HashMap<>();

    /**
     * 初始化
     */
    public BufferManager() {

    }

    /**
     * 获取所有的缓冲区的key的集合
     *
     * @return java.util.Set<java.lang.String>
     * @author Jerry.X.He
     * @date 5/5/2017 12:18 AM
     * @since 1.0
     */
    public Set<String> buffNames() {
        return new HashSet<>(bufferToBuffInfo.keySet());
    }

    /**
     * 创建一个缓冲区
     *
     * @param buffName          给定的buffName
     * @param outputPath        给定的输出文件的路径
     * @param charset           输出的字符集
     * @param threshold         输出的阈值
     * @param buffSizeEstimator buffSizeEstimate
     * @param handler           buffHandler
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 12:18 AM
     * @since 1.0
     */
    public void createAnBuffer(String buffName, String outputPath, String charset, int threshold,
                               BuffSizeEstimator buffSizeEstimator, BufferHandler handler) {
        assert0(buffName != null, "'buffName' can't be null ");
        assert0(outputPath != null, "'outputPath' can't be null ");
        assert0(charset != null, "'charset' can't be null ");
        assert0(threshold > 0, "'threshold' must > 0 ");
        assert0(buffSizeEstimator != null, "'buffSizeEstimator' can't be null ");
        assert0(handler != null, "'handler' can't be null ");

        if (bufExists(buffName)) {
            throw new RuntimeException("the buffInfo with key : " + buffName + " is already exists !");
        }

        BuffInfo buffInfo = new BuffInfo(outputPath, charset, threshold, buffSizeEstimator, handler);
        bufferToBuffInfo.put(buffName, buffInfo);
    }

    public void createAnBuffer(String buffName, String outputPath, String charset, int threshold, BuffSizeEstimator buffSizeEstimator) {
        createAnBuffer(buffName, outputPath, charset, threshold, buffSizeEstimator, DEFAULT_BUFF_HANDLER);
    }

    public void createAnBuffer(String buffName, String outputPath, String charset) {
        createAnBuffer(buffName, outputPath, charset, DEFAULT_BUFF_THRESHOLD, DEFAULT_BUFFSIZE_ESTIMATOR);
    }

    public void createAnBuffer(String buffName, String outputPath) {
        createAnBuffer(buffName, outputPath, Tools.DEFAULT_CHARSET);
    }

    /**
     * 如果buffName对应的缓冲不存在, 才创建Buffer
     *
     * @param buffName          给定的buffName
     * @param outputPath        给定的输出文件的路径
     * @param charset           输出的字符集
     * @param threshold         输出的阈值
     * @param buffSizeEstimator buffSizeEstimate
     * @param handler           buffHandler
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 12:20 AM
     * @since 1.0
     */
    public void createAnBufferIfNotExists(String buffName, String outputPath, String charset, int threshold,
                                          BuffSizeEstimator buffSizeEstimator, BufferHandler handler) {
        if (!bufExists(buffName)) {
            BuffInfo buffInfo = new BuffInfo(outputPath, charset, threshold, buffSizeEstimator, handler);
            bufferToBuffInfo.put(buffName, buffInfo);
        }
    }

    public void createAnBufferIfNotExists(String buffName, String outputPath, String charset, int threshold, BuffSizeEstimator buffSizeEstimator) {
        createAnBufferIfNotExists(buffName, outputPath, charset, threshold, buffSizeEstimator, DEFAULT_BUFF_HANDLER);
    }

    public void createAnBufferIfNotExists(String buffName, String outputPath, String charset, BufferHandler handler) {
        createAnBufferIfNotExists(buffName, outputPath, charset, DEFAULT_BUFF_THRESHOLD, DEFAULT_BUFFSIZE_ESTIMATOR, handler);
    }

    public void createAnBufferIfNotExists(String buffName, String outputPath, String charset) {
        createAnBufferIfNotExists(buffName, outputPath, charset, DEFAULT_BUFF_HANDLER);
    }

    public void createAnBufferIfNotExists(String buffName, String outputPath) {
        createAnBufferIfNotExists(buffName, outputPath, Tools.DEFAULT_CHARSET);
    }

    /**
     * 关闭给定的Buffer
     *
     * @param buffName 给定的buffName
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 12:20 AM
     * @since 1.0
     */
    public void closeAnBuffer(String buffName) throws Exception {
        flushBuffer(buffName, true);
    }

    /**
     * 关闭所有的Buffer
     *
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 12:20 AM
     * @since 1.0
     */
    public void closeAllBuffer() throws Exception {
        for (String buffName : buffNames()) {
            closeAnBuffer(buffName);
        }
    }

    /**
     * 判断给定的buffName的buffer是否存在
     *
     * @param buffName 给定的buffName
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 12:20 AM
     * @since 1.0
     */
    public boolean bufExists(String buffName) {
        return getBuffInfo(buffName) != null;
    }

    /**
     * 获取的buffName对应的BuffInfo
     *
     * @param buffName 给定的buffName
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 12:20 AM
     * @since 1.0
     */
    public BuffInfo getBuffInfo(String buffName) {
        return bufferToBuffInfo.get(buffName);
    }

    /**
     * 向给定的缓冲区中添加数据 并检测buffer中的数据是否超过了阈值
     *
     * @param buffName   给定的buffName
     * @param content    需要输出的content
     * @param appendCRLF 是否需要回车
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 12:22 AM
     * @since 1.0
     */
    public void appendBuffer(String buffName, String content, boolean appendCRLF) throws Exception {
        assert0(buffName != null, "'buffName' can't be null ");
        if (!bufExists(buffName)) {
            throw new RuntimeException("have no buffInfo with key : " + buffName + ", please createAnBuffer first !");
        }

        BuffInfo buffInfo = bufferToBuffInfo.get(buffName);
        buffInfo.getSb().append(content);
        if (appendCRLF) {
            buffInfo.getSb().append(Tools.CRLF);
        }
        if (buffInfo.getSb().length() >= buffInfo.getThreshold()) {
            buffInfo.getHandler().beforeHandle(buffInfo);
            synchronized (buffInfo.getSb()) {
                if (buffInfo.getSb().length() >= buffInfo.getThreshold()) {
                    // judge if 'buf' exists in case of 'MultiThreadConcurrent'
                    if (bufExists(buffName)) {
//						flushBuffer(buffInfo.sb, buffInfo.outputPath, buffInfo.charset, logFlags);
                        buffInfo.getHandler().handleBuffer(buffInfo);
                    } else {
                        Log.log("the buffer : '" + buffName + "' already be removed !");
                    }
                }
            }
            buffInfo.getHandler().afterHandle(buffInfo);
        }
    }

    public void appendBuffer(String buffName, String content) throws Exception {
        appendBuffer(buffName, content, false);
    }

    public void appendBufferCRLF(String buffName, String content) throws Exception {
        appendBuffer(buffName, content, true);
    }

    /**
     * 刷出缓存的数据
     *
     * @param buffName    给定的buffName
     * @param isLastBatch 是否刷出数据之后, 关闭缓冲
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 12:22 AM
     * @since 1.0
     */
    public void flushBuffer(String buffName, boolean isLastBatch) throws Exception {
        assert0(buffName != null, "'buffName' can't be null ");
        if (!bufExists(buffName)) {
            throw new RuntimeException("have no buffInfo with key : " + buffName + ", please createAnBuffer first !");
        }

        BuffInfo buffInfo = bufferToBuffInfo.get(buffName);
        if (buffInfo.getSb().length() > 0) {
            buffInfo.getHandler().beforeHandle(buffInfo);
            synchronized (buffInfo.getSb()) {
                if (buffInfo.getSb().length() > 0) {
                    // judge if 'buf' exists in case of 'MultiThreadConcurrent'
                    if (bufExists(buffName)) {
//						flushBuffer(buffInfo.sb, buffInfo.outputPath, buffInfo.charset, logFlags);
                        buffInfo.getHandler().handleBuffer(buffInfo);

                        if (isLastBatch) {
                            bufferToBuffInfo.remove(buffName);
                        }
                    } else {
                        Log.log("the buffer : '" + buffName + "' already be removed !");
                    }
                }
            }
            buffInfo.getHandler().afterHandle(buffInfo);
        }
    }

    public void flushBuffer(String buffName) throws Exception {
        flushBuffer(buffName, false);
    }

    // update the step 'flushDataToPath' into 'threadPoolExecutor'		at 2016.04.16

    /**
     * 刷出sb中的内容到给定的文件
     *
     * @param sb      需要写出的内容
     * @param path    需要输出的文件
     * @param charset 字节转换为字符的编码
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 12:23 AM
     * @since 1.0
     */
    public static void flushBuffer(final StringBuffer sb, final String path, final String charset) throws IOException {
        assert0(sb != null, "'sb' can't be null ");
        assert0(path != null, "'path' can't be null ");
        assert0(charset != null, "'charset' can't be null ");

        // move 'nextThree' a head incase of 'buff.sb.length >= buff.threshold', got an circle, but can't clear 'buff.sb'		at 2016.04.23
        long kbLength = Tools.getKBytesByBytes(sb.length());
        String content = sb.toString();
        sb.setLength(0);

        if (!Tools.THREAD_POOL.isShutdown()) {
            Tools.append(content, path, charset, true);
        } else {
            Tools.append(content, path, charset, false);
        }
    }

    public void flushBuffer(StringBuffer sb, String path) throws IOException {
        flushBuffer(sb, path, Tools.DEFAULT_CHARSET);
    }


}
